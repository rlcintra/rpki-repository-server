module RPKI.Repository.Data where

import Control.Concurrent
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified RPKI.Repository.Notification as N
import RPKI.Repository.Snapshot

data Config  = Config {
  publicationPath      :: FilePath,
  rrdpURI              :: String,
  rsyncMirroring       :: [String],
  rsyncPath            :: FilePath,
  rsyncCmd             :: Maybe String,
  rsyncUpdateFrequency :: Int, -- ^ in seconds
  rrdpPort             :: Int
}

type Hash = B.ByteString

type State = MVar StateValue

type StateValue = (N.Notification, Map.Map URI StateMapValue)

data StateMapValue = StateMapValue {
  publish :: Publish,
  hash    :: Hash
} deriving (Eq, Show)

data Diff = Added URI Hash ObjectPayload   -- ^ Object payload in base 64
          | Updated URI Hash ObjectPayload Hash -- ^ Object payload in base 64
          | Removed URI Hash
  deriving (Eq, Show)
