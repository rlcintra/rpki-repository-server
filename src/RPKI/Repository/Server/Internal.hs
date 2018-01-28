module RPKI.Repository.Server.Internal where

import RPKI.Repository.Data
import qualified RPKI.Repository.Snapshot as S
import qualified Data.ByteString.Char8 as B
import qualified RPKI.Repository.Delta as Delta
import qualified RPKI.Repository.Notification as N
import System.FilePath (pathSeparator)
import System.Directory (doesFileExist)

-- Paths
getNotificationPath :: Config -> FilePath
getNotificationPath c = publicationPath c +/ "notification.xml"

getSnapshotPath :: Config -> S.SessionId -> S.Serial -> FilePath
getSnapshotPath c sId serial = getSerialPath c (B.unpack sId ) serial +/ "snapshot.xml"

getPathForSnapshot :: Config -> S.Snapshot -> FilePath
getPathForSnapshot c s = getSnapshotPath c (S.sessionId s) (S.serial s)

doesNotificationFileExist :: Config -> IO Bool
doesNotificationFileExist = doesFileExist . getNotificationPath

getSessionPath :: Config -> N.SessionId -> FilePath
getSessionPath c sId = publicationPath c +/ sId

getSerialPath :: Config -> N.SessionId -> N.Serial -> FilePath
getSerialPath c sId serial = getSessionPath c sId +/show serial

getDeltaPath :: Config -> N.SessionId -> Delta.Serial -> FilePath
getDeltaPath c sId serial = getSerialPath c sId serial +/ "delta.xml"

getPathForDelta :: Config -> Delta.Delta -> FilePath
getPathForDelta c d = getDeltaPath c (Delta.sessionId d) (Delta.serial d)

(+/) :: String -> String -> String
(+/) s1 s2 = s1 ++ [pathSeparator] ++ s2
