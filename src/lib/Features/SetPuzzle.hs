module Features.SetPuzzle where

import           Application
import           Puzzle

import           Data.Aeson
import           Data.Text

newtype SetPuzzle =
  SetPuzzle Text
  deriving (Show, Eq)

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle <$> o .: "puzzle"

instance Command SetPuzzle where
  apply s (SetPuzzle p) = (s {puzzle = Just $ Puzzle p}, [])
