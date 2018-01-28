{-# LANGUAGE DuplicateRecordFields #-}
module ServerTest where

import Control.Exception (bracket)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import RPKI.Repository.Server
import RPKI.Repository.Server.Internal
import qualified RPKI.Repository.Delta as D
import qualified RPKI.Repository.Notification as N
import qualified RPKI.Repository.Snapshot as S
import RPKI.Repository.XML
import System.Directory
import System.IO.Temp
import Test.Tasty.Hspec

serverTests :: Spec
serverTests = do
  around withConfig $
    describe "Server - Rsync" $ do
      specify "Initialisation - empty rsync directory" $ \config -> do
        Right state <- initialiseState config
        rsyncUpdate config state
        notification <- getNotification config
        N.serial (notification :: N.Notification) `shouldBe` 1
        N.deltas notification `shouldBe` []
        snapshot <- getSnapshot config notification
        S.sessionId snapshot `shouldBe` (B.pack $ N.sessionId notification)
        S.serial snapshot `shouldBe` N.serial (notification :: N.Notification)
        S.publishs snapshot `shouldBe` []

      specify "Initialisation - non empty rsync directory" $ \config -> do
        copyFile file1 (rsyncPath config +/ "file1.dat")
        createDirectory $ rsyncPath config +/ "sub_dir"
        copyFile file2 (rsyncPath config +/ "sub_dir" +/ "file1.dat")
        Right state <- initialiseState config
        rsyncUpdate config state
        notification <- getNotification config
        N.serial (notification :: N.Notification) `shouldBe` 1
        N.deltas notification `shouldBe` []
        snapshot <- getSnapshot config notification
        S.sessionId snapshot `shouldBe` (B.pack $ N.sessionId notification)
        S.serial snapshot `shouldBe` (N.serial (notification :: N.Notification))
        length (S.publishs snapshot) `shouldBe` 2

      specify "Rsync update - new file" $ \config -> do
        copyFile file1 (rsyncPath config +/ "file1.dat")
        Right state <- initialiseState config
        rsyncUpdate config state
        notification <- getNotification config
        N.serial (notification :: N.Notification) `shouldBe` 1
        N.deltas notification `shouldBe` []
        snapshot <- getSnapshot config notification
        S.serial snapshot `shouldBe` (N.serial (notification :: N.Notification))
        length (S.publishs snapshot) `shouldBe` 1
        copyFile file2 (rsyncPath config +/ "file2.dat")
        rsyncUpdate config state
        notificationAfterUpdate <- getNotification config
        N.serial (notificationAfterUpdate :: N.Notification) `shouldBe` 2
        length (N.deltas notificationAfterUpdate) `shouldBe` 1
        snapshotAfterUpdate <- getSnapshot config notificationAfterUpdate
        S.sessionId snapshotAfterUpdate `shouldBe` (B.pack $ N.sessionId notificationAfterUpdate)
        S.serial snapshotAfterUpdate `shouldBe` (N.serial (notificationAfterUpdate :: N.Notification))
        length (S.publishs snapshotAfterUpdate) `shouldBe` 2

      specify "Rsync update - update file" $ \config -> do
        copyFile file1 (rsyncPath config +/ "file1.dat")
        createBigFile (rsyncPath config +/ "big_file.dat")
        Right state <- initialiseState config
        rsyncUpdate config state
        notification <- getNotification config
        N.serial (notification :: N.Notification) `shouldBe` 1
        N.deltas notification `shouldBe` []
        snapshot <- getSnapshot config notification
        S.serial snapshot `shouldBe` (N.serial (notification :: N.Notification))
        length (S.publishs snapshot) `shouldBe` 2
        copyFile file2 (rsyncPath config +/ "file1.dat")
        rsyncUpdate config state
        notificationAfterUpdate <- getNotification config
        N.serial (notificationAfterUpdate :: N.Notification) `shouldBe` 2
        length (N.deltas notificationAfterUpdate) `shouldBe` 1
        snapshotAfterUpdate <- getSnapshot config notificationAfterUpdate
        S.sessionId snapshotAfterUpdate `shouldBe` (B.pack $ N.sessionId notificationAfterUpdate)
        S.serial snapshotAfterUpdate `shouldBe` (N.serial (notificationAfterUpdate :: N.Notification))
        length (S.publishs snapshotAfterUpdate) `shouldBe` 2

      specify "Rsync update - delete file" $ \config -> do
        let file1Path = rsyncPath config +/ "file1.dat"
        copyFile file1 file1Path
        createBigFile (rsyncPath config +/ "big_file.dat")
        Right state <- initialiseState config
        rsyncUpdate config state
        notification <- getNotification config
        N.serial (notification :: N.Notification) `shouldBe` 1
        N.deltas notification `shouldBe` []
        snapshot <- getSnapshot config notification
        S.serial snapshot `shouldBe` (N.serial (notification :: N.Notification))
        length (S.publishs snapshot) `shouldBe` 2
        removeFile file1Path
        rsyncUpdate config state
        notificationAfterUpdate <- getNotification config
        N.serial (notificationAfterUpdate :: N.Notification) `shouldBe` 2
        length (N.deltas notificationAfterUpdate) `shouldBe` 1
        snapshotAfterUpdate <- getSnapshot config notificationAfterUpdate
        S.sessionId snapshotAfterUpdate `shouldBe` (B.pack $ N.sessionId notificationAfterUpdate)
        S.serial snapshotAfterUpdate `shouldBe` (N.serial (notificationAfterUpdate :: N.Notification))
        length (S.publishs snapshotAfterUpdate) `shouldBe` 1

-- Utility methods

withConfig :: (Config -> IO ()) -> IO ()
withConfig = bracket createConfig cleanConfig

createConfig :: IO Config
createConfig = do
  tempDir <- getTemporaryDirectory
  rsyncTempDir <- createTempDirectory tempDir "server-test-rsync"
  rsyncMirroringTempDir <- createTempDirectory tempDir "server-test-rsync-mirroring"
  publicationTempDir <- createTempDirectory tempDir "server-test-pub"
  return Config {
    publicationPath       = publicationTempDir,
    rrdpURI               = "https://test.net",
    rsyncMirroring        = [rsyncMirroringTempDir],
    rsyncPath             = rsyncTempDir,
    rsyncCmd              = Nothing,
    rsyncUpdateFrequency  = 60,
    rrdpPort              = 0
  }

cleanConfig :: Config -> IO ()
cleanConfig config = mapM_ removeDirectoryRecursive pathsToRemove
  where
    pathsToRemove = rsyncMirroring config ++ ([publicationPath, rsyncPath] <*> [config])


getNotification :: Config -> IO N.Notification
getNotification c = do
  eitherNotification <- loadNotification $ getNotificationPath c
  case eitherNotification of
    Right n -> return n
    Left e  -> fail e

getSnapshot:: Config -> N.Notification -> IO S.Snapshot
getSnapshot c n = do
  eitherS <- loadSnapshot $ getSnapshotPath c (B.pack $ N.sessionId n) (N.serial (n :: N.Notification))
  case eitherS of
    Right s -> return s
    Left e  -> fail e

file1, file2 :: FilePath
file1 = "tests/resources/file1.dat"
file2 = "tests/resources/file2.dat"

createBigFile :: FilePath -> IO ()
createBigFile fp = writeFile fp $ take 10000 (cycle "bla")
