{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.Repository.Notification where

type SessionId = String
type Serial = Int
type URI = String
type Hash = String

data Snapshot = Snapshot {
  uri          :: URI,
  hash         :: Hash
} deriving (Show, Read, Eq)

data Delta = Delta {
  serial       :: Serial,
  uri          :: URI,
  hash         :: Hash
} deriving (Show, Read, Eq)

data Notification = Notification {
  sessionId    :: SessionId,
  serial       :: Serial,
  snapshot     :: Snapshot,
  deltas       :: [Delta]
} deriving (Show, Read, Eq)
