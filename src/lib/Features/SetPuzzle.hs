module Features.SetPuzzle where

import           Data.Aeson
import           Data.Text

data SetPuzzle =
  SetPuzzle Text
  deriving (Show, Eq)

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle <$> o .: "puzzle"
