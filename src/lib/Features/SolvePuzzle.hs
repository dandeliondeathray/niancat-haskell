{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Features.SolvePuzzle where

import Application
import Data.Aeson
import Data.Map
import Data.Maybe
import TextShow
import Web
import Prelude hiding (Word, lookup)

data SubmitSolution
  = SubmitSolution User Word
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

registerSolver :: User -> Word -> NiancatState -> NiancatState
registerSolver u w s = s'
  where
    s' = if hasSolved u w s then s else  s { solvers = alter f w $ solvers s }
    f ss = case ss of
            Just ss' -> Just $ u : ss'
            Nothing -> Just [u]

hasSolved :: User -> Word -> NiancatState -> Bool
hasSolved u w = elem u . fromMaybe [] . lookup w . solvers 

solvePuzzle :: Dictionary -> SubmitSolution -> NiancatState -> (NiancatState, SolutionResponse)
solvePuzzle dict (SubmitSolution u w) s = (s', r)
  where
    s' = registerSolver u w s
    r = case currentPuzzle s of
      Just p -> if solves dict w p then Correct w else Incorrect w
      Nothing -> NotSet
