{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Persistence.Events where

import Data.Time

import Niancat.Domain

class StoredEvent e where
  pack :: (User, UTCTime, NiancatEvent) -> e
  event :: e -> NiancatEvent
  user :: e -> User
  timestamp :: e -> UTCTime

class StoredEvent e => Store s e where
  getAll :: s -> IO [e]
  getSince :: UTCTime -> s -> IO [e]
  append :: [e] -> s -> IO ()
