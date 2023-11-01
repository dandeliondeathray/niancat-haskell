module Niancat.State where

import Data.Default.Class
import Data.Map hiding (foldl)
import Data.Maybe
import Data.Text hiding (empty, foldl)
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Prelude hiding (Word, lookup)

data NiancatState = State
  { currentPuzzle :: Maybe Puzzle,
    solvers :: Map Word [User],
    pendingUnsolutions :: Map User Text
  }
  deriving (Show, Eq)

instance Default NiancatState where
  def =
    State
      { currentPuzzle = Nothing,
        solvers = empty,
        pendingUnsolutions = empty
      }

apply :: NiancatState -> WithUser NiancatEvent -> NiancatState
apply s (WithUser _ (PuzzleSet p)) = s {currentPuzzle = Just p}
apply s (WithUser _ (CorrectSolutionSubmitted _ (FirstTime False))) = s
apply s (WithUser u (CorrectSolutionSubmitted w (FirstTime True))) = registerSolver u w s
apply s (WithUser u (UnsolutionPending t _)) = addPendingUnsolution u t s
apply s (WithUser u (UnsolutionSaved _)) = removePendingUnsolution u s
apply s _ = s

applyAll :: NiancatState -> WithUser [NiancatEvent] -> NiancatState
applyAll s (WithUser u es) = foldl apply s (fmap (WithUser u) es)

registerSolver :: User -> Word -> NiancatState -> NiancatState
registerSolver u w s = s {solvers = alter appendUser w $ solvers s}
  where
    appendUser = Just . (u :) . fromMaybe []

firstTime :: User -> Word -> NiancatState -> FirstTime
firstTime u w = FirstTime . notElem u . fromMaybe [] . lookup w . solvers

addPendingUnsolution :: User -> Text -> NiancatState -> NiancatState
addPendingUnsolution u t s = s {pendingUnsolutions = insert u t $ pendingUnsolutions s}

removePendingUnsolution :: User -> NiancatState -> NiancatState
removePendingUnsolution u s = s {pendingUnsolutions = delete u $ pendingUnsolutions s}

pendingUnsolution :: User -> NiancatState -> Maybe Text
pendingUnsolution u = lookup u . pendingUnsolutions
