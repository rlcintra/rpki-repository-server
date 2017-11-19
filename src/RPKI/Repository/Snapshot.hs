{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.Repository.Snapshot where

type SessionId = String
type Serial = Int
type URI = String
type ObjectPayload = String

data Snapshot = Snapshot {
  sessionId    :: SessionId,
  serial       :: Serial,
  publishs     :: [Publish]
} deriving (Show, Read, Eq)

data Publish = Publish {
  uri          :: URI,
  object       :: ObjectPayload
} deriving (Show, Read, Eq)
