module RPKI.Repository.Rsync where

import Data.Maybe (catMaybes)
import Control.Monad
import GHC.IO.Handle (hGetContents)
import RPKI.Repository.Config
import System.Exit
import System.Process

updateRsyncMirrors :: Config -> IO [String]
updateRsyncMirrors c = do
  cmd <- getRsyncCmd c
  let rsyncList = rsyncMirroring c
  errors <- mapM (updateMirror cmd) rsyncList
  return $ catMaybes errors
  where updateMirror :: String -> String -> IO (Maybe String)
        updateMirror cmd rsyncURL = do
          (_, _, Just herr, process) <- createProcess (proc (cmd) ["-ar", "--delete", rsyncURL, getRsyncPath (rsyncPath c) rsyncURL]){std_err = CreatePipe}
          exitCode <- waitForProcess process
          case exitCode of
            ExitSuccess -> return Nothing
            ExitFailure _ -> liftM Just (hGetContents herr)

getRsyncCmd :: Config -> IO String
getRsyncCmd c = do
  let cmd = rsyncCmd c
  case rsyncCmd c of
    Just cmd -> return cmd
    _        -> readProcess "which" ["rsync"] "" >>= return . filter (\c -> c /= '\n')

getRsyncPath :: String -> String -> FilePath
getRsyncPath base url = base ++ "/" ++ sanatisedUrl
  where sanatisedUrl = drop 8 . map (\c -> if c == '/' then '_' else c) $ url -- TODO: use regex
