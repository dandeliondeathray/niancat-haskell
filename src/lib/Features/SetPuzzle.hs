{-# LANGUAGE MultiParamTypeClasses #-}

module Features.SetPuzzle where

import           Application
import           Web

import           Data.Aeson
import           TextShow

data SetPuzzle =
  SetPuzzle Puzzle
  deriving (Show, Eq)

data SetPuzzleResponse
  = PuzzleSet Puzzle
  | SamePuzzle Puzzle

instance FromRequest SetPuzzle where
  parse = jsonData

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle . Puzzle <$> o .: "puzzle"

instance Response SetPuzzleResponse where
  messages (PuzzleSet p) =
    [Reply "OK!", Notification $ mconcat ["Dagens nia är **", showtl p, "**"]]
  messages (SamePuzzle p) =
    [Reply $ mconcat ["Nian är redan satt till ", showtl p]]

instance Command SetPuzzle SetPuzzleResponse where
  apply (SetPuzzle p') s =
    case puzzle s of
      Just p
        | p /= p' -> (s', r)
      Nothing -> (s', r)
      Just p
        | p == p' -> (s, SamePuzzle p)
    where
      s' = s {puzzle = Just p'}
      r = PuzzleSet p'

setPuzzle :: Handler
setPuzzle = do
  command
    PUT
    "/v2/puzzle"
    (parse :: Parser SetPuzzle)
    (apply :: Applyer SetPuzzle SetPuzzleResponse)
