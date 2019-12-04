{-# LANGUAGE LambdaCase #-}

module Puzzle
  ( Puzzle,
    puzzle,
    Word,
    word,
    Key,
    key,
    wkey,
    pkey
  ) where

import           Data.List      (sort)
import           Data.Text.Lazy hiding (filter)
import           GHC.Exts       hiding (Word)
import           Prelude        hiding (Word, unwords)
import           TextShow

newtype Puzzle = Puzzle Text
puzzle :: Text -> Puzzle
puzzle = Puzzle . canonicalize

newtype Word = Word Text
word :: Text -> Word
word = Word

newtype Key = Key Text
key :: Text -> Key
key = Key . fromList . sort . toList . canonicalize
pkey :: Puzzle -> Key
pkey (Puzzle p) = key p
wkey :: Word -> Key
wkey (Word w) = key w

canonicalize :: Text -> Text
canonicalize = toUpper . clean . removeDiacritics . toCaseFold
  where
    removeDiacritics =
      Data.Text.Lazy.map
        (\case
             c'
               | c' `elem` ['á', 'à'] -> 'a'
               | c' `elem` ['é', 'è'] -> 'e'
               | otherwise -> c')
    disallowedChars = "[- _]" :: String
    clean :: Text -> Text
    clean = fromList . filter (`notElem` disallowedChars) . toList

instance Eq Puzzle where
  Puzzle a == Puzzle b = key a == key b
instance Eq Word where
  Word a == Word b = canonicalize a == canonicalize b
instance Eq Key where
  Key a == Key b = a == b

instance TextShow Puzzle where
  showb (Puzzle x) =
    fromLazyText . unwords . chunksOf 3 . canonicalize $ x
instance TextShow Word where
  showb (Word x) = fromLazyText . canonicalize $ x
instance TextShow Key where
  showb (Key x) = fromLazyText x
instance Show Puzzle where
  show = toString . showb
instance Show Word where
  show = toString . showb
instance Show Key where
  show = toString . showb

instance Ord Key where
  Key a <= Key b = a <= b

