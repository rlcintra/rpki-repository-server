module RPKI.Repository.Server
  (
    startServer,
    Config (..)
  ) where

import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import RPKI.Repository.Notification
import RPKI.Repository.XML
import Text.XML.HXT.Core

data Config  = Config {
  filePath :: FilePath,
  port     :: Int
}

startServer :: Config -> IO()
startServer config =
  let serverPort = port config
      contents = filePath config
      staticSettings = defaultWebAppSettings contents
      app = staticApp staticSettings
  in do -- runX (xunpickleDocument xpSnapshot
        --                         [withRemoveWS yes]
        --                         ((filePath config) ++ "/snapshot.xml")
        --       >>> arrIO (\n -> do {print n; return n;}))
        --run serverPort app
        snapshot <- loadSnapshot $ contents ++ "/snapshot.xml"
        print snapshot
        return ()
