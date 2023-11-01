module Niancat.Events where

import Data.Text hiding (elem, foldl)
import Niancat.Domain
import Niancat.Puzzle
import Prelude hiding (Word)

data NiancatEvent
  = PuzzleSet Puzzle
  | InvalidPuzzleSet Puzzle
  | SamePuzzleSet Puzzle
  | CorrectSolutionSubmitted Word FirstTime
  | IncorrectSolutionSubmitted Word
  | SolutionSubmittedWithNoPuzzleSet
  | UnsolutionSaved Text
  | UnsolutionPending Text Puzzle
  | UnsolutionSubmittedWithNoPuzzleSet
  deriving (Show, Eq)
