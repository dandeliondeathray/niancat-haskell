module Application
  ( module Application
  , module Puzzle
  ) where

import           Data.Default.Class
import           Data.Text

import           Puzzle

newtype User =
  User Text
  deriving (Show, Eq)

newtype NiancatState = State{puzzle :: Maybe Puzzle} deriving (Show, Eq)

instance Default NiancatState where
  def = State {puzzle = Nothing}
