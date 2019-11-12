module Features.SolvePuzzle where

import           Data.Aeson
import           Data.Text.Lazy

import           Application

data SubmitSolution =
  SubmitSolution User Text
  deriving (Show, Eq)

instance FromJSON SubmitSolution where
  parseJSON =
    withObject "solution" $ \v -> do
      user <- v .: "user"
      solution <- v .: "solution"
      return $ SubmitSolution (User user) solution
