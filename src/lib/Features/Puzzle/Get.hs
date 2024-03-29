{-# LANGUAGE DataKinds #-}

module Features.Puzzle.Get where

import Niancat.Puzzle
import Niancat.Replies
import Niancat.State
import TextShow

data GetPuzzle
  = GetPuzzle
  deriving (Show, Eq)

data PuzzleResponse
  = CurrentPuzzle Puzzle
  | NoPuzzle

instance Response PuzzleResponse where
  messages (CurrentPuzzle p) = [Reply . showt $ p]
  messages NoPuzzle = [Reply "Nian är inte satt."]

getPuzzle :: NiancatState -> PuzzleResponse
getPuzzle = maybe NoPuzzle CurrentPuzzle . currentPuzzle
