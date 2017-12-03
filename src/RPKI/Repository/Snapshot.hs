{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.Repository.Snapshot where

import qualified Data.ByteString.Char8 as B

type SessionId = B.ByteString
type Serial = Int
type URI = B.ByteString
type ObjectPayload = B.ByteString

data Snapshot = Snapshot {
  sessionId    :: SessionId,
  serial       :: Serial,
  publishs     :: [Publish]
} deriving (Show, Read, Eq)

data Publish = Publish {
  uri          :: URI,
  object       :: ObjectPayload
} deriving (Show, Read, Eq)
