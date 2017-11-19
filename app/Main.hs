module Main where

import RPKI.Repository.Server

main :: IO ()
main = do
  startServer $ Config "/home/rcintra/temp" 8080
