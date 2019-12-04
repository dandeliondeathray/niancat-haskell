module Application
  ( module Application
  , module Dictionary
  , module Puzzle
  ) where

import           Data.Default.Class
import           Data.Text

import           Dictionary
import           Puzzle

newtype User =
  User Text
  deriving (Show, Eq)

newtype NiancatState = State {
  currentPuzzle :: Maybe Puzzle
  } deriving (Show, Eq)

instance Default NiancatState where
  def = State {currentPuzzle = Nothing}
