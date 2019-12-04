{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Features.SolvePuzzle where

import           Application
import           Web

import           Data.Aeson
import           Prelude     hiding (Word)
import           TextShow

data SubmitSolution =
  SubmitSolution User Word
  deriving (Show, Eq)

instance FromJSON SubmitSolution where
  parseJSON =
    withObject "solution" $ \v -> do
      user <- v .: "user"
      solution <- v .: "solution"
      return $ SubmitSolution (User user) (word solution)

data SolutionResponse
  = Correct Word
  | Incorrect Word
  | NotSet

instance Response SolutionResponse where
  messages NotSet = [Reply "Nian är inte satt än!"]
  messages (Incorrect guess) = [Reply $ "Ordet " <> showt guess <> " finns inte med i SAOL."]
  messages (Correct guess) = [Reply $ "Ordet " <> showt guess <> " är korrekt!"]

solvePuzzle :: Dictionary -> SubmitSolution -> NiancatState -> (NiancatState, SolutionResponse)
solvePuzzle dict (SubmitSolution u w) s = (s', r)
  where
    s' = s
    r = case currentPuzzle s of
       Just p  -> if solves dict w p then Correct w else Incorrect w
       Nothing -> NotSet
