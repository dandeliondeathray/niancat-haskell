{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Features.SetPuzzle where

import           Application
import           Web

import           Data.Aeson
import           TextShow

newtype SetPuzzle = SetPuzzle Puzzle deriving (Show, Eq)

data SetPuzzleResponse
  = PuzzleSet Puzzle
  | InvalidPuzzle Puzzle
  | SamePuzzle Puzzle
  deriving (Show, Eq)

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle . puzzle <$> o .: "puzzle"

instance Response SetPuzzleResponse where
  messages (PuzzleSet p) =
    [Reply "OK!", Notification $ mconcat ["Dagens nia är **", showt p, "**"]]
  messages (SamePuzzle p) =
    [Reply $ mconcat ["Nian är redan satt till ", showt p]]
  messages (InvalidPuzzle p) =
    [Reply $ mconcat ["Pusslet " <> showt p <> " är inte giltigt!"]]

setPuzzle :: Dictionary -> SetPuzzle -> NiancatState -> (NiancatState, SetPuzzleResponse)
setPuzzle dict (SetPuzzle p') s =
  case currentPuzzle s of
    Just p
      | p == p' -> (s, SamePuzzle p)
      | not . valid dict $ p -> (s, InvalidPuzzle p)
      | otherwise -> (s', PuzzleSet p')
    Nothing -> (s', PuzzleSet p')
  where
    s' = s {currentPuzzle = Just p'}

