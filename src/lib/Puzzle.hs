module Puzzle
  ( Puzzle(Puzzle)
  ) where

import           Data.List      (sort)
import           Data.Text.Lazy hiding (filter)
import           GHC.Exts

data Puzzle =
  Puzzle Text

canonicalize :: Text -> Text
canonicalize = toUpper . clean . removeDiacritics . toCaseFold
  where
    removeDiacritics =
      Data.Text.Lazy.map
        (\c ->
           case c of
             c'
               | c' `elem` ['á', 'à'] -> 'a'
               | c' `elem` ['é', 'è'] -> 'e'
               | otherwise -> c')
    disallowedChars = "[- _]" :: String
    clean :: Text -> Text
    clean = fromList . filter (flip notElem disallowedChars) . toList

instance Eq Puzzle where
  Puzzle a == Puzzle b = shrink a == shrink b
    where
      shrink :: Text -> Text
      shrink = fromList . sort . toList . canonicalize

instance Show Puzzle where
  show (Puzzle x) = toList . intercalate " " . chunksOf 3 . canonicalize $ x
