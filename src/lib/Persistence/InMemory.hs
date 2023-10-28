{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Persistence.InMemory where

import Control.Concurrent.STM
import Data.List hiding (intercalate)
import Data.Ord

import Persistence.Events
import Niancat.Domain
import Data.Time (UTCTime)
import Data.Text hiding (pack, filter)
import Data.Aeson
import Niancat.Puzzle

newtype InMemoryStore = InMemory
  { events :: TVar [InMemoryEvent]
  }

instance Store InMemoryStore InMemoryEvent where
  getAll :: InMemoryStore -> IO [InMemoryEvent]
  getAll = fmap (sortBy (comparing timestamp)) . readTVarIO . events

  getSince :: UTCTime -> InMemoryStore -> IO [InMemoryEvent]
  getSince t = fmap (sortBy (comparing timestamp) . filter ((> t) . timestamp)) . readTVarIO . events

  append :: [InMemoryEvent] -> InMemoryStore -> IO ()
  append es s = atomically $ readTVar (events s) >>= writeTVar (events s) . (es ++)

data InMemoryEvent = IME User UTCTime NiancatEvent

instance StoredEvent InMemoryEvent where
  pack (u, t, e) = IME u t e
  event     (IME _ _ e) = e
  user      (IME u _ _) = u
  timestamp (IME _ t _) = t

eventType :: NiancatEvent -> Text
eventType (PuzzleSet _) = "puzzle-set"
eventType (InvalidPuzzleSet _) = "puzzle-set:invalid"
eventType (SamePuzzleSet _) = "puzzle-set:same"
eventType (CorrectSolutionSubmitted{}) = "solution-submitted:correct"
eventType (IncorrectSolutionSubmitted _) = "solution-submitted:incorrect"
eventType SolutionSubmittedWithNoPuzzleSet = "solution-submitted:no-puzzle-set"

eventData :: NiancatEvent -> Value
eventData (PuzzleSet p) = object ["puzzle" .= show p]
eventData (InvalidPuzzleSet p) = object ["puzzle" .= show p]
eventData (SamePuzzleSet p) = object ["puzzle" .= show p]
eventData (CorrectSolutionSubmitted (Word w) f) = object ["word" .= w, "first-time" .= isFirstTime f]
eventData (IncorrectSolutionSubmitted w) = object ["word" .= show w]
eventData SolutionSubmittedWithNoPuzzleSet = object []

instance ToJSON InMemoryEvent where
  toJSON (IME (User u) t e) =
    object
      [ "type" .= intercalate "/" [eventType e, "v1"]
      , "timestamp" .= toJSON t
      , "user" .= u
      , "data" .= eventData e
      ]
