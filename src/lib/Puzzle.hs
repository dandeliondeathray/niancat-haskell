module Puzzle
  ( Puzzle(Puzzle)
  ) where

import           Data.List (sort)
import           Data.Text hiding (filter)
import           GHC.Exts

data Puzzle =
  Puzzle Text

canonicalize :: Text -> Text
canonicalize = sorted . clean . removeDiacritics . toCaseFold
  where
    sorted = fromList . sort . toList
    removeDiacritics =
      Data.Text.map
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
  Puzzle a == Puzzle b = canonicalize a == canonicalize b

instance Show Puzzle where
  show (Puzzle x) = show . intercalate " " . chunksOf 3 . canonicalize $ x
