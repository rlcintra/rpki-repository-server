{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.Repository.State where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import RPKI.Repository.Snapshot

type State = Map.Map URI StateValue

type Hash = BS.ByteString

data StateValue = StateValue {
  publish :: Publish,
  hash    :: Hash
}

data Diff = Added URI Hash ObjectPayload
          | Updated URI Hash ObjectPayload
          | Removed URI


