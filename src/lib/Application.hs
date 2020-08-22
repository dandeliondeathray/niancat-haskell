module Application
  ( module Application,
    module Dictionary,
    module Puzzle,

    NiancatState (solvers, currentPuzzle)
  )
where

import Prelude hiding (Word)

import Data.Aeson
import Data.Default.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Dictionary
import Puzzle
import TextShow

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

instance ToJSON NiancatState where
  toJSON s = object ["puzzle" .= p]
    where
      p' = currentPuzzle s :: Maybe Puzzle
      p = fmap showt p'
