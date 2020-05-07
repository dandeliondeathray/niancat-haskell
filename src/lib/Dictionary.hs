module Dictionary where

import           Puzzle

import           Data.Foldable       (toList)
import           Data.List           (nub)
import           Data.Map            (Map, map, member, (!?))
import           Data.Maybe
import           Data.NonEmpty.Mixed (groupKey)
import           Data.Text.Lazy      (pack)
import           GHC.Exts            hiding (Word, toList)
import           Prelude             hiding (Word, map)

newtype Dictionary = Dictionary (Map Key [Word])

build :: [String] -> Dictionary
build = Dictionary
  . map toList
  . fromList
  . groupKey wkey
  . nub
  . fmap (word . pack)
  . filter ((== 9) . length)

has :: Dictionary -> Word -> Bool
has (Dictionary d) w = maybe False (elem w) $ d !? wkey w

valid :: Dictionary -> Puzzle -> Bool
valid (Dictionary d) p = pkey p `member` d

solves :: Dictionary -> Word -> Puzzle -> Bool
solves dictionary w p = wkey w == pkey p && has dictionary w

