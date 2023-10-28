{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Persistence.Events where

import Data.Time

import Niancat.Domain
import Data.Text
import Data.Aeson
import Niancat.Puzzle

data EventMetadata = Meta User UTCTime

data EventWithMeta = Imbued NiancatEvent EventMetadata

imbue :: User -> UTCTime -> NiancatEvent -> EventWithMeta
imbue u t e = Imbued e (Meta u t)

event :: EventWithMeta -> NiancatEvent
event (Imbued e _) = e

user :: EventWithMeta -> User
user (Imbued _ (Meta u _)) = u

timestamp :: EventWithMeta -> UTCTime
timestamp (Imbued _ (Meta _ t)) = t

class Store s where
  getAll :: s -> IO [EventWithMeta]
  getSince :: UTCTime -> s -> IO [EventWithMeta]
  append :: [EventWithMeta] -> s -> IO ()


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

instance ToJSON EventWithMeta where
  toJSON (Imbued e (Meta (User u) t)) =
    object
      [ "type" .= intercalate "/" [eventType e, "v1"]
      , "timestamp" .= toJSON t
      , "user" .= u
      , "data" .= eventData e
      ]
