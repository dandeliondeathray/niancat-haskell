module Application
  ( module Application
  , module Dictionary
  , module Puzzle
  ) where

import           Data.Aeson
import           Data.Default.Class
import           Data.Text
import           TextShow

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

instance ToJSON NiancatState where
  toJSON s = object ["puzzle" .= p]
    where
      p' = currentPuzzle s :: Maybe Puzzle
      p = fmap showt p'
