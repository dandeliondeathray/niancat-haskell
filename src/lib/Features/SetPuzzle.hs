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
setPuzzle dict (WithUser (u, SetPuzzle p)) s = withUser u $ set (currentPuzzle s) (valid dict p)
 where
  set _ False = [InvalidPuzzleSet p]
  set (Just p') True | p' == p = [SamePuzzleSet p']
  set Nothing True = [PuzzleSet p]
  set (Just _) True = [PuzzleSet p]
