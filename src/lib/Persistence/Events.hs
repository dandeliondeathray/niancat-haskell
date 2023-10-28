module Persistence.Events where

import Control.Concurrent.STM
import Data.Aeson
import Data.Text (Text, intercalate)
import Data.Time

import Niancat.Domain
import Niancat.Puzzle

newtype StoredEvent = StoredEvent (NiancatEvent, User, UTCTime)
event :: StoredEvent -> NiancatEvent
event (StoredEvent(e, _, _)) = e
user :: StoredEvent -> User
user (StoredEvent(_, u, _)) = u
timestamp :: StoredEvent -> UTCTime
timestamp (StoredEvent(_, _, t)) = t

class Store s where
  getAll :: s -> IO [StoredEvent]
  getSince :: UTCTime -> s -> IO [StoredEvent]
  append :: UTCTime -> User -> [NiancatEvent] -> s -> IO ()

type EventStream = TVar [StoredEvent]

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

instance ToJSON StoredEvent where
  toJSON (StoredEvent (e, User u, t)) =
    object
      [ "type" .= intercalate "/" [eventType e, "v1"]
      , "timestamp" .= toJSON t
      , "user" .= u
      , "data" .= eventData e
      ]
