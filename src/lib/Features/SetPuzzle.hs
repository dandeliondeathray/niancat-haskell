{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Features.SetPuzzle where

import Data.Aeson

import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle

newtype SetPuzzle = SetPuzzle Puzzle deriving (Show, Eq)

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle . puzzle <$> o .: "puzzle"

setPuzzle :: Dictionary -> SetPuzzle -> NiancatState -> [NiancatEvent]
setPuzzle dict (SetPuzzle p') s =
  case currentPuzzle s of
    Just p
      | p == p' -> [SamePuzzleSet p]
      | not . valid dict $ p -> [InvalidPuzzleSet p]
      | otherwise -> [PuzzleSet p']
    Nothing -> [PuzzleSet p']
