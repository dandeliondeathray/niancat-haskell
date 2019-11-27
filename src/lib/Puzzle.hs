{-# LANGUAGE LambdaCase #-}

module Puzzle
  ( Puzzle(Puzzle),
    Word(Word),
    solves
  ) where

import           Data.List      (sort)
import           Data.Text.Lazy hiding (filter)
import           GHC.Exts       hiding (Word)
import           Prelude        hiding (Word, unwords)
import           TextShow

newtype Puzzle = Puzzle Text
newtype Word = Word Text

solves :: Word -> Puzzle -> Bool
solves (Word w) (Puzzle p) = canonicalize w == canonicalize p

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
  Puzzle a == Puzzle b = shrink a == shrink b
instance Eq Word where
  Word a == Word b = shrink a == shrink b

instance TextShow Puzzle where
  showb (Puzzle x) =
    fromLazyText . unwords . chunksOf 3 . canonicalize $ x
instance TextShow Word where
  showb (Word x) = fromLazyText . canonicalize $ x
instance Show Puzzle where
  show = toString . showb
instance Show Word where
  show = toString . showb

shrink :: Text -> Text
shrink = fromList . sort . toList . canonicalize
