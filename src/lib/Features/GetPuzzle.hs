{-# LANGUAGE DataKinds #-}

module Features.GetPuzzle where

import TextShow

import Niancat.Domain
import Niancat.Replies
import Niancat.Puzzle

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
