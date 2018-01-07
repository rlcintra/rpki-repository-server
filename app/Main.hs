module Main where

import RPKI.Repository.Data
import RPKI.Repository.Server

main :: IO ()
main = do
  startServer $ Config {
    publicationPath = "/home/rcintra/temp/rpki_contents",
    rrdpURI = "http://localhost:8080",
    rsyncMirroring = ["rsync://rpki.apnic.net/repository", "rsync://rpki.apnic.net/member_repository"],
    rsyncPath = "/home/rcintra/temp/rsync_test",
    rsyncCmd = Nothing,
    rsyncUpdateFrequency = 60,
    rrdpPort = 8080
    }
