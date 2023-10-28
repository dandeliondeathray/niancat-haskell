{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Persistence.InMemory where

import Control.Concurrent.STM
import Data.List hiding (intercalate)
import Data.Ord

import Persistence.Events
import Data.Time (UTCTime)

newtype InMemoryStore = InMemory
  { events :: TVar [EventWithMeta]
  }

instance Store InMemoryStore where
  getAll :: InMemoryStore -> IO [EventWithMeta]
  getAll = fmap (sortBy (comparing timestamp)) . readTVarIO . events

  getSince :: UTCTime -> InMemoryStore -> IO [EventWithMeta]
  getSince t = fmap (sortBy (comparing timestamp) . filter ((> t) . timestamp)) . readTVarIO . events

  append :: [EventWithMeta] -> InMemoryStore -> IO ()
  append es s = atomically $ readTVar (events s) >>= writeTVar (events s) . (es ++)
