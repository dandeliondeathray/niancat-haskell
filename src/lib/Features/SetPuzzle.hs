{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Features.SetPuzzle where

import Application
import Data.Aeson
import TextShow
import Web

newtype SetPuzzle = SetPuzzle Puzzle deriving (Show, Eq)

data SetPuzzleEvent
  = PuzzleSet Puzzle
  | InvalidPuzzle Puzzle
  | SamePuzzle Puzzle
  deriving (Show, Eq)

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle . puzzle <$> o .: "puzzle"

instance Event SetPuzzleEvent where
  apply s (PuzzleSet p) = s {currentPuzzle = Just p}
  apply s _ = s

instance Response SetPuzzleEvent where
  messages (PuzzleSet p) =
    [Reply "OK!", Notification $ mconcat ["Dagens nia är **", showt p, "**"]]
  messages (SamePuzzle p) =
    [Reply $ mconcat ["Nian är redan satt till ", showt p]]
  messages (InvalidPuzzle p) =
    [Reply $ mconcat ["Pusslet " <> showt p <> " är inte giltigt!"]]

setPuzzle :: Dictionary -> SetPuzzle -> NiancatState -> [SetPuzzleEvent]
setPuzzle dict (SetPuzzle p') s =
  case currentPuzzle s of
    Just p
      | p == p' -> [SamePuzzle p]
      | not . valid dict $ p -> [InvalidPuzzle p]
      | otherwise -> [PuzzleSet p']
    Nothing -> [PuzzleSet p']