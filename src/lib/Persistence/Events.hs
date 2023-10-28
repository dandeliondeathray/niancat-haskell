module Persistence.Events where

import Data.Time

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Niancat.Domain
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

class ToEventWithMeta a where
  unmarshal :: a -> Either String EventWithMeta
class FromEventWithMeta a where
  marshal :: EventWithMeta -> a

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

parserFor :: Text -> Either String (Value -> Parser NiancatEvent)
parserFor t = do
  p <- case t of
    "puzzle-set" -> Right $ \v -> PuzzleSet . puzzle <$> v .: "puzzle"
    "puzzle-set:invalid" -> Right $ \v -> InvalidPuzzleSet . puzzle <$> v .: "puzzle"
    "puzzle-set:same" -> Right $ \v -> SamePuzzleSet . puzzle <$> v .: "puzzle"
    "solution-submitted:correct" -> Right $ \v -> do
      w <- Word <$> v .: "word"
      CorrectSolutionSubmitted w <$> (FirstTime <$> v .: "first-time")
    "solution-submitted:incorrect" -> Right $ \v -> IncorrectSolutionSubmitted . Word <$> v .: "word"
    "solution-submitted:no-puzzle" -> Right $ \_ -> return SolutionSubmittedWithNoPuzzleSet
    _ -> Left ("invalid event type: " ++ show t)
  return $ withObject (show t) p

instance ToJSON EventWithMeta where
  toJSON (Imbued e (Meta (User u) t)) =
    object
      [ "timestamp" .= t
      , "user" .= u
      , "eventType" .= eventType e
      , "eventData" .= eventData e
      ]

instance FromJSON EventWithMeta where
  parseJSON = withObject "event" $ \root -> do
    ts <- root .: "timestamp"
    u <- root .: "user"
    t <- root .: "eventType"
    d <- root .: "eventData"
    e <- parseData t d
    return $ Imbued e (Meta (User u) ts)

parseData :: Text -> Value -> Parser NiancatEvent
parseData "puzzle-set" = withObject "puzzle-set" $ \v -> PuzzleSet . puzzle <$> v .: "puzzle"
parseData _ = error "unknown event type"
