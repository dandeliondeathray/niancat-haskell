{-# LANGUAGE InstanceSigs #-}

module Niancat.Domain where

import Prelude hiding (Word)

import Data.Default.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text hiding (elem, foldl)

import Data.String
import Niancat.Puzzle

newtype User
  = User Text
  deriving (Show, Eq)

newtype WithUser a = WithUser (User, a) deriving (Show, Eq)
withUser :: User -> a -> WithUser a
withUser = curry WithUser
withoutUser :: WithUser a -> a
withoutUser (WithUser (_, a)) = a

instance IsString User where
  fromString :: String -> User
  fromString = User . fromString

data NiancatState = State
  { currentPuzzle :: Maybe Puzzle
  , solvers :: Map Word [User]
  }
  deriving (Show, Eq)

instance Default NiancatState where
  def = State{currentPuzzle = Nothing, solvers = Map.empty}

newtype FirstTime = FirstTime Bool
  deriving (Show, Eq)
isFirstTime :: FirstTime -> Bool
isFirstTime (FirstTime b) = b

data NiancatEvent
  = PuzzleSet Puzzle
  | InvalidPuzzleSet Puzzle
  | SamePuzzleSet Puzzle
  | CorrectSolutionSubmitted Word FirstTime
  | IncorrectSolutionSubmitted Word
  | SolutionSubmittedWithNoPuzzleSet
  deriving (Show, Eq)

apply :: NiancatState -> WithUser NiancatEvent -> NiancatState
apply s (WithUser (_, PuzzleSet p)) = s{currentPuzzle = Just p}
apply s (WithUser (_, CorrectSolutionSubmitted _ (FirstTime False))) = s
apply s (WithUser (u, CorrectSolutionSubmitted w (FirstTime True))) = registerSolver u w s
apply s _ = s

applyAll :: NiancatState -> WithUser [NiancatEvent] -> NiancatState
applyAll s (WithUser (u, es)) = foldl apply s (fmap (withUser u) es)

registerSolver :: User -> Word -> NiancatState -> NiancatState
registerSolver u w s = s{solvers = Map.alter appendUser w $ solvers s}
 where
  appendUser = Just . (u :) . fromMaybe []

firstTime :: User -> Word -> NiancatState -> FirstTime
firstTime u w = FirstTime . notElem u . fromMaybe [] . Map.lookup w . solvers
