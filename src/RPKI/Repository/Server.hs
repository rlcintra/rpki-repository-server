module RPKI.Repository.Server
  (
    startServer,
    Config (..)
  ) where

import Network.Wai.Application.Static

data Config  = Config {
  filePath :: String,
  port     :: Int
}

startServer :: Config -> IO()
startServer config =
  putStr "Hello world"
