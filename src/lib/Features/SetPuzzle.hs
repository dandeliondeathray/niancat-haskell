{-# LANGUAGE FlexibleInstances #-}

module Features.SetPuzzle where

import Data.Aeson
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Niancat.State

newtype SetPuzzle = SetPuzzle Puzzle deriving (Show, Eq)

instance FromJSON (WithUser SetPuzzle) where
  parseJSON = withObject "puzzle" $ \o -> do
    p <- puzzle <$> o .: "puzzle"
    u <- User <$> o .: "user"
    return $ WithUser u $ SetPuzzle p

setPuzzle :: Dictionary -> WithUser SetPuzzle -> NiancatState -> WithUser [NiancatEvent]
setPuzzle dict (WithUser u (SetPuzzle p)) s = WithUser u $ set (currentPuzzle s) (valid dict p)
  where
    set _ False = [InvalidPuzzleSet p]
    set (Just p') True | p' == p = [SamePuzzleSet p']
    set Nothing True = [PuzzleSet p]
    set (Just _) True = [PuzzleSet p]
