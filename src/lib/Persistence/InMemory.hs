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
  empty = do
    es <- newTVarIO []
    return InMemory{events = es}

  getAll = fmap (sortBy (comparing timestamp)) . readTVarIO . events

  getSince t = fmap (sortBy (comparing timestamp) . filter ((> t) . timestamp)) . readTVarIO . events

  append s t u es = atomically $ do
    es' <- readTVar $ events s
    let es'' = es' ++ ((,u,t) <$> es)
    writeTVar (events s) es''
