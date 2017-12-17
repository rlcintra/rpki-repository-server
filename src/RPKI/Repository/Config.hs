module RPKI.Repository.Config where

data Config  = Config {
  publicationPath :: FilePath,
  rrdpURI         :: String,
  rsyncMirroring  :: [String],
  rsyncPath       :: FilePath,
  rsyncCmd        :: Maybe String,
  rrdpPort        :: Int
}
