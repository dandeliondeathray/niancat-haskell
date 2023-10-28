module Persistence.InMemory where

import Control.Concurrent.STM
import Data.List hiding (intercalate)
import Data.Ord

import Persistence.Events

newtype InMemoryStore = InMemory
  { events :: TVar [EventWithMeta]
  }

instance Store InMemoryStore where
  getAll = fmap (sortBy (comparing timestamp)) . readTVarIO . events

  getSince t = fmap (sortBy (comparing timestamp) . filter ((> t) . timestamp)) . readTVarIO . events

  append es s = atomically $ readTVar (events s) >>= writeTVar (events s) . (es ++)
