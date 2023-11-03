{-# LANGUAGE FlexibleInstances #-}

module Features.Unsolutions where

import Data.Aeson
import Data.Map hiding (foldl)
import Data.Text hiding (elem, foldl)
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Niancat.State
import Persistence.Events
import Prelude hiding (words)

newtype SubmitUnsolution
  = SubmitUnsolution Text
  deriving (Show, Eq)

instance FromJSON SubmitUnsolution where
  parseJSON = withObject "unsolution" $ \v -> SubmitUnsolution <$> (v .: "text")

submitUnsolution :: User -> SubmitUnsolution -> NiancatState -> WithUser [NiancatEvent]
submitUnsolution u (SubmitUnsolution t) s =
  case currentPuzzle s of
    Just p -> WithUser u $
      case (t, pendingUnsolution u s) of
        ("", Just t') -> [UnsolutionSaved t']
        _ -> if isFuzzyMatch then [UnsolutionSaved t] else [UnsolutionPending t p]
          where
            isFuzzyMatch = p `elem` fmap (puzzle . canonicalize) (words t)
    Nothing -> WithUser u [UnsolutionSubmittedWithNoPuzzleSet]

newtype Unsolutions = Unsolutions (Map User [Text])

showUnsolutionsForUser :: User -> [EventWithMeta] -> Unsolutions
showUnsolutionsForUser u = Unsolutions . foldl (flip march) mempty
  where
    march (Imbued (UnsolutionSaved s) (Meta u' _)) | u == u' = insertWith (++) u [s]
    march _ = id
