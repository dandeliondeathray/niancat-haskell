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

newtype FirstTime = FirstTime Bool
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
apply s (CorrectSolutionSubmitted _ _ (FirstTime False)) = s
apply s (CorrectSolutionSubmitted w u (FirstTime True)) = registerSolver u w s
apply s _ = s

registerSolver :: User -> Word -> NiancatState -> NiancatState
registerSolver u w s = s { solvers = Map.alter appendUser w $ solvers s }
  where appendUser = Just . (u :) . fromMaybe []

firstTime :: User -> Word -> NiancatState -> FirstTime
firstTime u w = FirstTime . notElem u . fromMaybe [] . Map.lookup w . solvers
