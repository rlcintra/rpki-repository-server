{-# LANGUAGE DuplicateRecordFields #-}
module RPKI.Repository.Server
  (
    startServer,
    Config (..)
  ) where

import Control.Concurrent
import Crypto.Hash as C
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Time.Clock
import Data.UUID (toString)
import qualified Data.UUID.V4 as UUIDv4
import Control.Monad (forM, void)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import RPKI.Repository.Data
import qualified RPKI.Repository.Delta as Delta
import qualified RPKI.Repository.Notification as N
import qualified RPKI.Repository.Snapshot as S
import RPKI.Repository.Rsync
import RPKI.Repository.XML
import System.Directory (doesFileExist, createDirectoryIfMissing, createDirectory, getFileSize)
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
        case state of
          Left errorMsg -> log'' logger2 errorMsg
          Right s       -> void (forkIO (startPeriodicRsyncUpdate config s))
        flushLogStr logger
        run serverPort app

initialiseState :: Config -> IO (Either String State)
initialiseState c =
  do notificationFileExist <- doesNotificationFileExist c
     if not notificationFileExist
     then Right <$> initialiseServerSession c
     else do notification <- loadNotification $ notificationPath c
             case notification of
               Left msg -> return $ Left msg
               Right n -> do snapshot <- loadSnapshot $
                                snapshotPath c (B.pack $ N.sessionId n) (N.serial (n :: N.Notification))
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
     createDirectoryIfMissing True $ sessionPath c sessionId ++ "/" ++ show serial
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
     hash <- writeSnapshot s (snapshotPath c sId serial)
     return (s, hash)

createNotification :: Config -> String -> N.SessionId -> N.Serial -> S.Snapshot -> [N.Delta] -> IO N.Notification
createNotification c hash sId serial snapshot deltas =
  do cleanedUpDeltas <- cleanupDeltas
     let nSnapshot = N.Snapshot {
                       N.uri  = sURI,
                       N.hash = hash
                     }
     let n = N.Notification {
               N.sessionId = sId,
               N.serial    = serial,
               N.snapshot  = nSnapshot,
               N.deltas    = cleanedUpDeltas
             }
     writeNotification n (notificationPath c)
     return n
     where sURI = snapshotURI c (B.pack sId) serial
           cleanupDeltas = do
             snapshotSize <- getFileSize $ getPathForSnapshot c snapshot
             deltaSizes <- mapM (\d -> getFileSize $ deltaPath c sId (N.serial (d :: N.Delta))) deltas
             return $ take (length $ takeWhile (<= snapshotSize) $ scanr (+) 0 deltaSizes) deltas

stateMapFromSnapshot :: S.Snapshot -> Map.Map S.URI StateMapValue
stateMapFromSnapshot s =
  foldr (\p l -> Map.insert (S.uri p)
    (StateMapValue p (B.pack $ show (C.hash (Base64.decodeLenient $ S.object p) :: Digest SHA256))) l)
    Map.empty (S.publishs s)

startPeriodicRsyncUpdate :: Config -> State -> IO ()
startPeriodicRsyncUpdate c s = do
  t1 <- getCurrentTime
  recurrentRsyncUpdate c s
  t2 <- getCurrentTime
  let delay = rsyncUpdateFrequency c - round (diffUTCTime t2 t1)
  if delay <= 0
  then print "Warning: Rsync update delay is too short." -- TODO: use proper logging
  else threadDelay $ delay * 1000 * 1000
  startPeriodicRsyncUpdate c s

recurrentRsyncUpdate :: Config -> State -> IO ()
recurrentRsyncUpdate c s = do
  print "Sync thread running" -- TODO: remove
  _ <- updateRsyncMirrors c
  (notification, sMap) <- takeMVar s
  diffs <- diff c sMap
  sv <- applyDiffs c (notification, sMap) diffs
  putMVar s sv

applyDiffs :: Config -> StateValue -> [Diff] -> IO StateValue
applyDiffs _ sv [] = return sv
applyDiffs c sv@(n, sMap) diffs = do
  createDirectory $ serialPath c sId serial
  (delta, dHash) <- createDelta
  let updatedMap = foldr updateMap sMap diffs
  (s, sh) <- createSnapshot c (B.pack sId) serial (publish <$> Map.elems updatedMap)
  let nDeltaUri = deltaURI c (B.pack sId) serial
  let nDelta = N.Delta serial nDeltaUri dHash
  updatedNotification <- createNotification c sh sId serial s (nDelta : N.deltas n)
  return (updatedNotification, updatedMap)
  where serial = N.serial (n :: N.Notification) + 1
        (ps, ws) = foldr convert ([],[]) diffs
        sId = N.sessionId n
        createDelta = do
          let delta = Delta.Delta (N.sessionId (n :: N.Notification)) serial ps ws
          dHash <- writeDelta delta (deltaPath c (N.sessionId n) serial)
          return (delta, dHash)
        convert (Added u h p) (ps, ws) = (Delta.Publish u p Nothing : ps, ws)
        convert (Updated u h p oldH) (ps, ws) = (Delta.Publish u p (Just oldH) : ps, ws)
        convert (Removed u h) (ps, ws) = (ps, Delta.Withdraw u h : ws)
        updateMap (Added u h p) m = Map.insert u (StateMapValue (S.Publish u p) h) m
        updateMap (Updated u h p _) m = Map.insert u (StateMapValue (S.Publish u p) h) m
        updateMap (Removed u h) m = Map.delete u m

-- Paths
notificationPath :: Config -> FilePath
notificationPath c = publicationPath c ++ "/notification.xml"

snapshotPath :: Config -> S.SessionId -> S.Serial -> FilePath
snapshotPath c sId serial = serialPath c (B.unpack sId ) serial ++ "/snapshot.xml"

getPathForSnapshot :: Config -> S.Snapshot -> FilePath
getPathForSnapshot c s = snapshotPath c (S.sessionId s) (S.serial s)

doesNotificationFileExist :: Config -> IO Bool
doesNotificationFileExist = doesFileExist . notificationPath

sessionPath :: Config -> N.SessionId -> FilePath
sessionPath c sId = publicationPath c ++ "/" ++ sId

serialPath :: Config -> N.SessionId -> N.Serial -> FilePath
serialPath c sId serial = sessionPath c sId ++ "/" ++ show serial

deltaPath :: Config -> N.SessionId -> Delta.Serial -> FilePath
deltaPath c sId serial = serialPath c sId serial ++ "/delta.xml"

getPathForDelta :: Config -> Delta.Delta -> FilePath
getPathForDelta c d = deltaPath c (Delta.sessionId d) (Delta.serial d)

-- URIs
snapshotURI :: Config -> S.SessionId -> S.Serial -> N.URI
snapshotURI c sID serial = rrdpURI c ++ "/" ++ B.unpack sID ++ "/" ++ show serial ++ "/snapshot.xml"

deltaURI :: Config -> S.SessionId -> S.Serial -> N.URI
deltaURI c sID serial = rrdpURI c ++ "/" ++ B.unpack sID ++ "/" ++ show serial ++ "/delta.xml"

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
