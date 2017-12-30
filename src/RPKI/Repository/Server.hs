{-# LANGUAGE DuplicateRecordFields #-}
module RPKI.Repository.Server
  (
    startServer,
    Config (..)
  ) where

import Control.Concurrent
import Control.Monad
import Crypto.Hash as C
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.UUID (toString)
import qualified Data.UUID.V4 as UUIDv4
import Control.Monad (forM)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import RPKI.Repository.Data
import qualified RPKI.Repository.Delta as Delta
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
        state <- initialiseState config
        flushLogStr logger
        --run serverPort app

initialiseState :: Config -> IO (Either String State)
initialiseState c =
  do notificationFileExist <- doesNotificationFileExist c
     if not notificationFileExist
     then Right <$> initialiseServerSession c
     else do notification <- loadNotification $ notificationPath c
             case notification of
               Left msg -> return $ Left msg
               Right n -> do snapshot <- loadSnapshot $ snapshotPath c (B.pack $ N.sessionId n)
                             case snapshot of
                               Left msg' -> return $ Left msg'
                               Right s   -> Right <$> newMVar (n, stateMapFromSnapshot s)

initialiseServerSession :: Config -> IO State
initialiseServerSession c =
  do sessionId <- toString <$> UUIDv4.nextRandom
     let serial = 1
     updateMirrorErrors <- updateRsyncMirrors c
     --forM updateMirrorErrors putStrLn -- TODO: use logger
     smvs <- getInitialStateMapValues c
     createDirectoryIfMissing True $ sessionPath c sessionId
     (snapshot, hash) <- createSnapshot c (B.pack sessionId) serial (map publish smvs)
     notification <- createNotification c hash sessionId serial snapshot []
     newMVar (notification,
              foldr (\v l -> Map.insert (S.uri . publish $ v) v l) (Map.empty :: Map.Map S.URI StateMapValue) smvs)

getInitialStateMapValues :: Config -> IO [StateMapValue]
getInitialStateMapValues c = catMaybes . fmap convert <$> diff c Map.empty
  where convert :: Diff -> Maybe StateMapValue
        convert d = case d of
                      Added u h p -> Just $ StateMapValue (S.Publish u p) h
                      _ -> Nothing

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

stateMapFromSnapshot :: S.Snapshot -> Map.Map S.URI StateMapValue
stateMapFromSnapshot s =
  foldr (\p l -> Map.insert (S.uri p) (StateMapValue p (B.pack $ show (C.hash (S.object p) :: Digest SHA256))) l) Map.empty (S.publishs s)

recurrentRsyncUpdate :: Config -> State -> IO ()
recurrentRsyncUpdate c s = do
  _ <- updateRsyncMirrors c
  (notification, sMap) <- takeMVar s
  diffs <- diff c sMap
  sv <- applyDiffs c (notification, sMap) diffs
  putMVar s sv

applyDiffs :: Config -> StateValue -> [Diff] -> IO StateValue
applyDiffs _ sv [] = return sv
applyDiffs c sv@(n, sMap) diffs = do
  delta <- createDelta
  writeDelta delta (rrdpURI c)
  return undefined
  where createDelta = do
          let serial = (N.serial n) + 1
          let (ps, ws) = foldr convert ([],[]) diffs
          return
          return ()
        convert (Added u h p) (ps, ws) = ((Delta.Publish u p Nothing) : ps, ws)
        convert (Updated u h p) (ps, ws) = ((Delta.Publish u p (Just h)) : ps, ws)
        convert (Added u h p) (ps, ws) = ((ps, (Delta.Withdraw u h) : ws)

-- Paths
notificationPath :: Config -> FilePath
notificationPath c = publicationPath c ++ "/notification.xml"

snapshotPath :: Config -> S.SessionId -> FilePath
snapshotPath c sId = sessionPath c (B.unpack sId) ++ "/snapshot.xml"

doesNotificationFileExist :: Config -> IO Bool
doesNotificationFileExist = doesFileExist . notificationPath

sessionPath :: Config -> N.SessionId -> FilePath
sessionPath c sId = publicationPath c ++ "/" ++ sId

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
