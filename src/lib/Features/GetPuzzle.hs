{-# LANGUAGE MultiParamTypeClasses #-}

module Features.GetPuzzle where

import           Application
import           Puzzle
import           Web

import           Data.Text.Lazy

data GetPuzzle =
  GetPuzzle

data PuzzleResponse =
  PuzzleResponse (Maybe Puzzle)

instance Query GetPuzzle PuzzleResponse where
  resolve _ = PuzzleResponse . puzzle

instance Response PuzzleResponse where
  messages (PuzzleResponse (Just p)) = [Reply . pack . show $ p]
  messages (PuzzleResponse Nothing)  = [Reply . pack $ "Nian är inte satt."]

instance FromRequest GetPuzzle where
  parse = return GetPuzzle

getPuzzle :: Handler
getPuzzle =
  query
    GET
    "/v2/puzzle"
    (parse :: Parser GetPuzzle)
    (resolve :: Resolver GetPuzzle PuzzleResponse)
