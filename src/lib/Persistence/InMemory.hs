{-# LANGUAGE TupleSections #-}

module Persistence.InMemory where

import Control.Concurrent.STM
import Data.List
import Data.Ord

import Persistence.Events

newtype InMemoryStore = InMemory
  { events :: EventStream
  }

instance Store InMemoryStore where
  getAll = fmap (sortBy (comparing timestamp)) . readTVarIO . events

  getSince t = fmap (sortBy (comparing timestamp) . filter ((> t) . timestamp)) . readTVarIO . events

  append t u es s = atomically $ do
    es' <- readTVar $ events s
    let es'' = es' ++ (StoredEvent . (,u,t) <$> es)
    writeTVar (events s) es''
