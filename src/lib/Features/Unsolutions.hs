{-# LANGUAGE FlexibleInstances #-}

module Features.Unsolutions where

import Data.Aeson
import Data.Text hiding (elem)
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Niancat.State
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
