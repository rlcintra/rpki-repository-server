{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Data.Yaml as Yaml
import Options.Applicative as Opt
import RPKI.Repository.Data
import RPKI.Repository.Server
import System.Directory (doesFileExist)

main :: IO ()
main = do
  configFile <- execParser opts
  configFileExists <- doesFileExist configFile
  eitherConfig <- if configFileExists
                    then loadConfig configFile
                    else return $ Left $ "Configuration file not found: " ++ configFile
  case eitherConfig of
    Right config -> startServer config
    Left err     -> putStrLn $ "Error: " ++ err ++ "\nExecute the program with -h for help."
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Starts the repository server with the given configuration file."
     <> header "RPKI Repository Server" )

-- Command line options

optionsParser :: Opt.Parser String
optionsParser =
  option auto
    ( long "config"
   <> short 'c'
   <> help "Server configuration file"
   <> showDefault
   <> value "config.yml"
   <> metavar "CONFIG_FILE" )

-- Config

loadConfig :: FilePath -> IO (Either String Config)
loadConfig fp = do config <- decodeFileEither fp
                   return $ case config of
                              Left e -> Left $ show e
                              Right c -> Right c

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
    v .:   "publicationPath"      <*>
    v .:   "rrdpURI"              <*>
    v .:   "rsyncMirroring"       <*>
    v .:   "rsyncPath"            <*>
    v .:?  "rsyncCmd"             <*>
    v .:   "rsyncUpdateFrequency" <*>
    v .:   "rrdpPort"
  parseJSON _ = fail "Expected Object for Config value"
