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

instance FromJSON (WithUser SubmitUnsolution) where
  parseJSON =
    withObject "unsolution" $ \v -> do
      txt <- v .: "text"
      usr <- v .: "user"
      return $ WithUser (User usr) $ SubmitUnsolution txt

submitUnsolution :: WithUser SubmitUnsolution -> NiancatState -> WithUser [NiancatEvent]
submitUnsolution (WithUser u (SubmitUnsolution t)) s =
  case currentPuzzle s of
    Just p -> WithUser u $
      case (t, pendingUnsolution u s) of
        ("", Just t') -> [UnsolutionSaved t']
        _ -> if isFuzzyMatch then [UnsolutionSaved t] else [UnsolutionPending t p]
          where
            isFuzzyMatch = p `elem` fmap (puzzle . canonicalize) (words t)
    Nothing -> WithUser u [UnsolutionSubmittedWithNoPuzzleSet]
