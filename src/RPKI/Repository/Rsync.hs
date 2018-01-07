module RPKI.Repository.Rsync (updateRsyncMirrors, diff ) where

import Conduit
import qualified Crypto.Hash as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.List ((\\))
import qualified Data.Set as Set
import Data.Maybe (catMaybes, isJust)
import Control.Monad
import GHC.IO.Handle (hGetContents)
import RPKI.Repository.Data
import RPKI.Repository.Snapshot (URI, ObjectPayload)
import System.Directory
import System.Exit
import System.Process

updateRsyncMirrors :: Config -> IO [String]
updateRsyncMirrors c = do
  cmd <- getRsyncCmd c
  let rsyncList = rsyncMirroring c
  errors <- mapM (updateMirror cmd) rsyncList
  return $ catMaybes errors
  where updateMirror :: String -> String -> IO (Maybe String)
        updateMirror cmd rsyncURL = do
          let rsyncPath' = getRsyncPath (rsyncPath c) rsyncURL
          createDirectoryIfMissing True rsyncPath'
          (_, _, Just herr, process) <- createProcess (proc cmd ["-ar", "--delete", rsyncURL, rsyncPath'])
                                                      {std_err = CreatePipe}
          exitCode <- waitForProcess process
          case exitCode of
            ExitSuccess -> return Nothing
            ExitFailure _ -> Just <$> hGetContents herr

getRsyncCmd :: Config -> IO String
getRsyncCmd c = do
  let cmd = rsyncCmd c
  case rsyncCmd c of
    Just cmd -> return cmd
    _        -> filter (/= '\n') <$> readProcess "which" ["rsync"] ""

getRsyncPath :: String -> String -> FilePath
getRsyncPath base url = base ++ "/" ++ sanatisedUrl
  where sanatisedUrl = url \\ "rsync://"

getURIFromRsyncPath :: String -> FilePath -> URI
getURIFromRsyncPath base fp = C8.pack $ "rsync://" ++ (fp \\ base)

diff :: Config -> Map.Map URI StateMapValue -> IO [Diff]
diff c s = do
  rsyncURIs <- runConduitRes $ sourceDirectoryDeep True (rsyncPath c) .| mapC (getURIFromRsyncPath rsyncBase) .| sinkList
  let uriSet = Set.fromList rsyncURIs
  added <- getAdded uriSet
  updated <- getUpdated uriSet
  let deleted = getDeleted uriSet
  return $ added ++ updated ++ deleted
  where rsyncBase = rsyncPath c
        stateURIs = Map.keysSet s
        getAdded :: Set.Set URI -> IO [Diff]
        getAdded uriSet' = do
          let addedURIs = Set.toList $ Set.difference uriSet' stateURIs
          loadedURIs <- mapM loadURI addedURIs
          return $ map (\(u, h, p) -> Added u h (Base64.encode p)) loadedURIs
        getUpdated :: Set.Set URI -> IO [Diff]
        getUpdated uriSet' = do
          let intersection = Set.toList $ Set.intersection uriSet' stateURIs
          loadedURIs <- mapM loadURI intersection
          return $ foldr f [] loadedURIs
          where f (u, h, p) accum =
                  if h == oldHash
                  then accum
                  else Updated u h (Base64.encode p) oldHash : accum
                  where oldHash = hash (s ! u)
        getDeleted :: Set.Set URI -> [Diff]
        getDeleted uriSet' = map (\uri -> Removed uri (hash $ s ! uri)) difference
          where difference = Set.toList $ Set.difference stateURIs uriSet'
        loadURI :: URI -> IO (URI, Hash, ObjectPayload)
        loadURI uri = do
          let fp = getRsyncPath rsyncBase (C8.unpack uri)
          payload <- BS.readFile fp
          let fileHash = C8.pack $ show (C.hash payload :: C.Digest C.SHA256)
          return (uri, fileHash, payload)

