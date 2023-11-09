module Niancat.Dictionary where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.List (nub)
import Data.Map (Map, elems, map, member, (!?))
import Data.Maybe
import Data.NonEmpty.Mixed (groupKey)
import Data.Text (pack)
import GHC.Exts hiding (Word, toList)
import Niancat.Puzzle
import Prelude hiding (Word, map)

newtype Dictionary = Dictionary (Map Key [Word])

build :: [String] -> Dictionary
build =
  Dictionary
    . map toList
    . fromList
    . fmap (second (nub . toList))
    . groupKey wkey
    . fmap (Word . pack)
    . filter ((== 9) . length)

has :: Dictionary -> Word -> Bool
has (Dictionary d) w = maybe False (elem w) $ d !? wkey w

valid :: Dictionary -> Puzzle -> Bool
valid (Dictionary d) p = pkey p `member` d

solves :: Dictionary -> Word -> Puzzle -> Bool
solves dictionary w p = wkey w == pkey p && has dictionary w

instance Show Dictionary where
  show (Dictionary d) =
    "dictionary of "
      ++ (show . length $ d)
      ++ " puzzles"
      ++ " ("
      ++ (show . sum . fmap length . elems $ d)
      ++ " unique words)"
