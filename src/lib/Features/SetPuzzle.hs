{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Features.SetPuzzle where

import Data.Aeson

import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle

newtype SetPuzzle = SetPuzzle Puzzle deriving (Show, Eq)

instance FromJSON (WithUser SetPuzzle) where
  parseJSON = withObject "puzzle" $ \o -> do
    p <- puzzle <$> o .: "puzzle"
    u <- User <$> o .: "user"
    return $ withUser u $ SetPuzzle p

setPuzzle :: Dictionary -> WithUser SetPuzzle -> NiancatState -> WithUser [NiancatEvent]
setPuzzle dict (WithUser (u, SetPuzzle p')) s =
  withUser u $ case currentPuzzle s of
    Just p
      | p == p' -> [SamePuzzleSet p]
      | not . valid dict $ p -> [InvalidPuzzleSet p]
      | otherwise -> [PuzzleSet p']
    Nothing -> [PuzzleSet p']
