module Main where

import RPKI.Repository.Server as Server

main :: IO ()
main = do
  Server.startServer $ Config "bla" 80
