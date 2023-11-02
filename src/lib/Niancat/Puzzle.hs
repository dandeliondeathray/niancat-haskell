module Niancat.Puzzle
  ( Puzzle,
    puzzle,
    canonicalize,
    Word (..),
    Key,
    key,
    wkey,
    pkey,
  )
where

import Data.List (sort)
import Data.Text
import Data.Text.Normalize
import GHC.Exts hiding (Word)
import TextShow
import Prelude hiding (Word, elem, filter, map, unwords)

newtype Puzzle = Puzzle Text

puzzle :: Text -> Puzzle
puzzle = Puzzle . canonicalize

newtype Word = Word Text

newtype Key = Key Text

key :: Text -> Key
key = Key . fromList . sort . toList . canonicalize

pkey :: Puzzle -> Key
pkey (Puzzle p) = key p

wkey :: Word -> Key
wkey (Word w) = key w

canonicalize :: Text -> Text
canonicalize = toUpper . normalize NFKC . filter (`notElem` (diacritics ++ separators)) . normalize NFKD
  where
    separators = "[- _]"
    diacritics = "\x0300\x0301\x0302\x0304"

instance Eq Puzzle where
  Puzzle a == Puzzle b = key a == key b

instance Eq Word where
  Word a == Word b = canonicalize a == canonicalize b

instance Ord Word where
  Word a <= Word b = canonicalize a <= canonicalize b

instance Eq Key where
  Key a == Key b = a == b

instance TextShow Puzzle where
  showb (Puzzle x) =
    fromText . unwords . chunksOf 3 . canonicalize $ x

instance TextShow Word where
  showb (Word x) = fromText . canonicalize $ x

instance TextShow Key where
  showb (Key x) = fromText x

instance Show Puzzle where
  show = toString . showb

instance Show Word where
  show = toString . showb

instance Ord Key where
  Key a <= Key b = a <= b

instance Show Key where
  show = toString . showb
