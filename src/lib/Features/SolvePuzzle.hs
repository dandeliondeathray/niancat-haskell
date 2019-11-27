{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Features.SolvePuzzle where

import           Application
import           Web

import           Data.Aeson
import           Prelude     hiding (Word)
import           Servant
import           TextShow

data SubmitSolution =
  SubmitSolution User Word
  deriving (Show, Eq)

instance FromJSON SubmitSolution where
  parseJSON =
    withObject "solution" $ \v -> do
      user <- v .: "user"
      solution <- v .: "solution"
      return $ SubmitSolution (User user) (Word solution)

data SolutionResponse
  = Correct
  | Incorrect Word
  | NotSet

instance Response SolutionResponse where
  messages NotSet = [Reply "Nian är inte satt än!"]
  messages (Incorrect guess) = [Reply $ "Ordet " <> showt guess <> " finns inte med i SAOL."]

solvePuzzle :: SubmitSolution -> NiancatState -> (NiancatState, SolutionResponse)
solvePuzzle (SubmitSolution u w) s = (s', r)
  where
    s' = s
    r = case puzzle s of
       Just p  -> if solves w p then Correct else Incorrect w
       Nothing -> NotSet

type SolvePuzzleAPI = "v2" :> "solutions" :> ReqBody '[JSON] SubmitSolution :> Post '[JSON] [Message]
solvePuzzleAPI :: Proxy SolvePuzzleAPI
solvePuzzleAPI = Proxy
