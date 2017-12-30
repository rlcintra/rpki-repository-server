module RPKI.Repository.Data where

import Control.Concurrent
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified RPKI.Repository.Notification as N
import RPKI.Repository.Snapshot

data Config  = Config {
  publicationPath :: FilePath,
  rrdpURI         :: String,
  rsyncMirroring  :: [String],
  rsyncPath       :: FilePath,
  rsyncCmd        :: Maybe String,
  rrdpPort        :: Int
}

type Hash = B.ByteString

type State = MVar StateValue

type StateValue = (N.Notification, Map.Map URI StateMapValue)

data StateMapValue = StateMapValue {
  publish :: Publish,
  hash    :: Hash
}

data Diff = Added URI Hash ObjectPayload
          | Updated URI Hash ObjectPayload
          | Removed URI
