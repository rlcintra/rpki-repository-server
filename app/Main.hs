module Main where

import RPKI.Repository.Config
import RPKI.Repository.Server

main :: IO ()
main = do
  startServer $ Config {
    publicationPath = "/home/rcintra/temp/rpki_contents",
    rrdpURI = "https://rrdp.raf.net",
    rsyncMirroring = ["rsync://rpki.apnic.net/repository"],
    rsyncPath = "/home/rcintra/temp/rsync_test",
    rsyncCmd = Nothing,
    rrdpPort = 8080
    }
