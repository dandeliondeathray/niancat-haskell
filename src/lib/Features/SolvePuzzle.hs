{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Features.SolvePuzzle where

import Data.Aeson
import Data.Maybe
import Prelude hiding (Word, lookup)

import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle

data SubmitSolution
  = SubmitSolution User Word
  deriving (Show, Eq)

instance FromJSON SubmitSolution where
  parseJSON =
    withObject "solution" $ \v -> do
      user <- v .: "user"
      solution <- v .: "solution"
      return $ SubmitSolution (User user) (word solution)

solvePuzzle :: Dictionary -> SubmitSolution -> NiancatState -> [NiancatEvent]
solvePuzzle dict (SubmitSolution u w) s =
  case currentPuzzle s of
      Just p -> if solves dict w p 
                then [CorrectSolutionSubmitted w u True]
                else [IncorrectSolutionSubmitted w]
                  where
                    first = not $ hasSolved u w s
      Nothing -> [SolutionSubmittedWithNoPuzzleSet]
