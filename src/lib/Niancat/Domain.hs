module Niancat.Domain where

import Prelude hiding (Word)

import Data.Default.Class
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.Text hiding (elem)

import Niancat.Puzzle


newtype User
  = User Text
  deriving (Show, Eq)

data NiancatState = State
  { currentPuzzle :: Maybe Puzzle
  , solvers :: Map Word [User]
  }
  deriving (Show, Eq)

instance Default NiancatState where
  def = State {currentPuzzle = Nothing, solvers = Map.empty}

data FirstTime = Yes | No
  deriving (Show, Eq)

data NiancatEvent
  = PuzzleSet Puzzle
  | InvalidPuzzleSet Puzzle
  | SamePuzzleSet Puzzle
  | CorrectSolutionSubmitted Word User FirstTime
  | IncorrectSolutionSubmitted Word
  | SolutionSubmittedWithNoPuzzleSet
  deriving (Show, Eq)

apply :: NiancatState -> NiancatEvent -> NiancatState
apply s (PuzzleSet p) = s { currentPuzzle = Just p }
apply s (CorrectSolutionSubmitted _ _ No) = s
apply s (CorrectSolutionSubmitted w u Yes) = registerSolver u w s
apply s _ = s

registerSolver :: User -> Word -> NiancatState -> NiancatState
registerSolver u w s = s'
  where
    s' = if hasSolved u w s then s else  s { solvers = Map.alter f w $ solvers s }
    f ss = case ss of
            Just ss' -> Just $ u : ss'
            Nothing -> Just [u]

hasSolved :: User -> Word -> NiancatState -> Bool
hasSolved u w = elem u . fromMaybe [] . Map.lookup w . solvers
