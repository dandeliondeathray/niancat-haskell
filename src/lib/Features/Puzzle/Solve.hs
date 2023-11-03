{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Features.Puzzle.Solve where

import Data.Aeson
import Data.Maybe
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Niancat.State
import Prelude hiding (Word, lookup)

newtype SubmitSolution = SubmitSolution Word deriving (Show, Eq)

instance FromJSON (WithUser SubmitSolution) where
  parseJSON = withObject "withUser" $ \v ->
    WithUser . User <$> v .: "user" <*> (SubmitSolution . Word <$> v .: "solution")

solvePuzzle :: Dictionary -> WithUser SubmitSolution -> NiancatState -> WithUser [NiancatEvent]
solvePuzzle dict (WithUser u (SubmitSolution w)) s =
  WithUser u $ case currentPuzzle s of
    Just p ->
      if solves dict w p
        then [CorrectSolutionSubmitted w $ firstTime u w s]
        else [IncorrectSolutionSubmitted w]
    Nothing -> [SolutionSubmittedWithNoPuzzleSet]
