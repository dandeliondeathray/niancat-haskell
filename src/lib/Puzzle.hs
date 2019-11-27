{-# LANGUAGE LambdaCase #-}

module Puzzle
  ( Puzzle(Puzzle)
  ) where

import           Data.List      (sort)
import           Data.Text.Lazy hiding (filter)
import           GHC.Exts
import           Prelude        hiding (unwords)
import           TextShow

newtype Puzzle =
  Puzzle Text

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
    where
      shrink :: Text -> Text
      shrink = fromList . sort . toList . canonicalize

instance TextShow Puzzle where
  showb (Puzzle x) =
    fromLazyText . unwords . chunksOf 3 . canonicalize $ x

instance Show Puzzle where
  show = toString . showb
