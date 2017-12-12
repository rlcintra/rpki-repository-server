module Main where

import RPKI.Repository.Server

main :: IO ()
main = do
  startServer $ Config "/home/rcintra/temp/rpki_contents" 8080
