{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.Repository.Delta where

import qualified Data.ByteString.Char8 as B

type SessionId = B.ByteString
type Serial = Int
type URI = B.ByteString
type ObjectPayload = B.ByteString
type Hash = String

data Delta = Delta {
  sessionId    :: SessionId,
  serial       :: Serial,
  publishs     :: [Publish],
  withdraws    :: [Withdraw]
} deriving (Show, Read, Eq)

data Publish = Publish {
  uri          :: URI,
  object       :: ObjectPayload,
  hash         :: Maybe Hash
} deriving (Show, Read, Eq)

data Withdraw = Withdraw {
  uri          :: URI,
  hash         :: Hash
} deriving (Show, Read, Eq)



