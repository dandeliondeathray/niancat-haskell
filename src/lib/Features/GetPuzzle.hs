{-# LANGUAGE MultiParamTypeClasses #-}

module Features.GetPuzzle where

import           Application
import           Puzzle
import           Web

import           Data.Maybe
import           Data.Text.Lazy
import           TextShow

data GetPuzzle =
  GetPuzzle

data PuzzleResponse
  = CurrentPuzzle Puzzle
  | NoPuzzle

instance Query GetPuzzle PuzzleResponse where
  resolve _ = fromMaybe NoPuzzle . fmap CurrentPuzzle . puzzle

instance Response PuzzleResponse where
  messages (CurrentPuzzle p) = [Reply . showtl $ p]
  messages NoPuzzle          = [Reply . pack $ "Nian är inte satt."]

instance FromRequest GetPuzzle where
  parse = return GetPuzzle

getPuzzle :: Handler
getPuzzle =
  query
    GET
    "/v2/puzzle"
    (parse :: Parser GetPuzzle)
    (resolve :: Resolver GetPuzzle PuzzleResponse)
