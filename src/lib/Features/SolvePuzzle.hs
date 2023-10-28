{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Features.SolvePuzzle where

import Data.Aeson
import Data.Maybe
import Prelude hiding (Word, lookup)

import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle

newtype SubmitSolution = SubmitSolution Word deriving (Show, Eq)

instance FromJSON (WithUser SubmitSolution) where
  parseJSON =
    withObject "solution" $ \v -> do
      u <- User <$> v .: "user"
      solution <- v .: "solution"
      return $ withUser u $ SubmitSolution (Word solution)

solvePuzzle :: Dictionary -> WithUser SubmitSolution -> NiancatState -> WithUser [NiancatEvent]
solvePuzzle dict (WithUser (u, SubmitSolution w)) s =
  withUser u $ case currentPuzzle s of
      Just p -> if solves dict w p
                then [CorrectSolutionSubmitted w $ firstTime u w s]
                else [IncorrectSolutionSubmitted w]
      Nothing -> [SolutionSubmittedWithNoPuzzleSet]
