{-# LANGUAGE DuplicateRecordFields #-}
module RPKI.Repository.Server
  (
    startServer,
    Config (..)
  ) where

import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Data.UUID (toString)
import qualified Data.UUID.V4 as UUIDv4
import Control.Monad (forM)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import RPKI.Repository.Config
import qualified RPKI.Repository.Notification as N
import qualified RPKI.Repository.Snapshot as S
import RPKI.Repository.Rsync
import RPKI.Repository.XML
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Log.FastLogger
import System.Log.FastLogger.Date
import Text.XML.HXT.Core

startServer :: Config -> IO()
startServer config =
  let serverPort = rrdpPort config
      contents = publicationPath config
      staticSettings = defaultWebAppSettings contents
      app = staticApp staticSettings
  in do logger <- setupLogger
        (logger2, closeLogger2) <- setupLogger'
        log'' logger2 "Initisalising server"
        status <- loadCurrentStatus config
        (notification, snapshot) <- case status of
                                      Right (n, s) -> return (n, s)
                                      Left msg -> do
                                        log'' logger2 msg
                                        initialiseServerSession config
        flushLogStr logger


    -- runX (xunpickleDocument xpSnapshot
        --                         [withRemoveWS yes]
        --                         ((publicationPath config) ++ "/snapshot.xml")
        --       >>> arrIO (\n -> do {print n; return n;}))
        --run serverPort app
--         notification <- runX $ xunpickleDocument xpNotification
--                                          [withRemoveWS yes]
--                                          ((publicationPath config) ++ "/notification.xml")
--         print notification
--         snapshot <- loadSnapshot $ contents ++ "/snapshot.xml"
--         case snapshot of
--           Left e  -> print e
--           Right s -> writeSnapshot s (contents ++ "/snapshot.out")
--         --print snapshot
--         return ()


loadCurrentStatus :: Config -> IO (Either String (N.Notification, S.Snapshot))
loadCurrentStatus c =
  do notificationFileExist <- doesNotificationFileExist c
     if not notificationFileExist
     then return $ Left "Notification files does not exist"
     else do notification <- loadNotification $ notificationPath c
             case notification of
               Left msg -> return $ Left msg
               Right n -> do snapshot <- loadSnapshot $ snapshotPath c (B.pack $ N.sessionId n)
                             case snapshot of
                               Left msg' -> return $ Left msg'
                               Right s   -> return (Right (n,s))

initialiseServerSession :: Config -> IO ((N.Notification, S.Snapshot))
initialiseServerSession c =
  do sessionId <- toString <$> UUIDv4.nextRandom
     let serial = 1
     updateMirrorErrors <- updateRsyncMirrors c
     forM updateMirrorErrors putStrLn -- TODO: use logger
     publishList <- getInitialPublishList c
     createDirectoryIfMissing True $ sessionPath c sessionId
     (snapshot, hash) <- createSnapshot c (B.pack sessionId) serial publishList
     notification <- createNotification c hash sessionId serial snapshot []
     return (notification, snapshot)

getInitialPublishList :: Config -> IO [S.Publish]
getInitialPublishList c = return [] -- TODO

createSnapshot :: Config -> S.SessionId -> S.Serial -> [S.Publish] -> IO (S.Snapshot, String)
createSnapshot c sId serial ps =
  do let s = S.Snapshot {
               S.sessionId = sId,
               S.serial    = serial,
               S.publishs  = ps
            }
     hash <- writeSnapshot s (snapshotPath c sId)
     return (s, hash)

createNotification :: Config -> String -> N.SessionId -> N.Serial -> S.Snapshot -> [N.Delta] -> IO N.Notification
createNotification c hash sId serial snapshot deltas =
  do let nSnapshot = N.Snapshot {
                       N.uri  = snapshotPath c (B.pack sId),
                       N.hash  = hash
                     }
     let n = N.Notification {
               N.sessionId = sId,
               N.serial    = serial,
               N.snapshot  = nSnapshot,
               N.deltas    = deltas
             }
     writeNotification n (notificationPath c)
     return n

-- Paths
notificationPath :: Config -> FilePath
notificationPath c = (publicationPath c) ++ "/notification.xml"

snapshotPath :: Config -> S.SessionId -> FilePath
snapshotPath c sId = (sessionPath c $ B.unpack sId) ++ "/snapshot.xml"

doesNotificationFileExist :: Config -> IO Bool
doesNotificationFileExist = doesFileExist . notificationPath

sessionPath :: Config -> N.SessionId -> FilePath
sessionPath c sId = (publicationPath c) ++ "/" ++ sId

-- Utils
setupLogger :: IO LoggerSet
setupLogger = newFileLoggerSet defaultBufSize "/tmp/rpki-repository-server.log"

setupLogger' :: IO (TimedFastLogger, IO ())
setupLogger' = do
  timeCache <- newTimeCache simpleTimeFormat
  newTimedFastLogger timeCache (LogFileNoRotate "/tmp/rpki-repository-server2.log" defaultBufSize)

log' :: LoggerSet -> String -> IO ()
log' ls msg = pushLogStrLn ls $ toLogStr msg
log'' :: ToLogStr msg => TimedFastLogger -> msg -> IO ()
log'' logger msg = logger $ \ft -> toLogStr ft <> toLogStr ": " <> toLogStr msg <> toLogStr "\n"
