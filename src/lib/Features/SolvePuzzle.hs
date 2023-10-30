{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Features.SolvePuzzle where

import Data.Aeson
import Data.Maybe
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle
import Prelude hiding (Word, lookup)

newtype SubmitSolution = SubmitSolution Word deriving (Show, Eq)

instance FromJSON (WithUser SubmitSolution) where
  parseJSON =
    withObject "solution" $ \v -> do
      u <- User <$> v .: "user"
      solution <- v .: "solution"
      return $ WithUser u $ SubmitSolution (Word solution)

solvePuzzle :: Dictionary -> WithUser SubmitSolution -> NiancatState -> WithUser [NiancatEvent]
solvePuzzle dict (WithUser u (SubmitSolution w)) s =
  WithUser u $ case currentPuzzle s of
    Just p ->
      if solves dict w p
        then [CorrectSolutionSubmitted w $ firstTime u w s]
        else [IncorrectSolutionSubmitted w]
    Nothing -> [SolutionSubmittedWithNoPuzzleSet]
