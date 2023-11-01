module Niancat.Domain where

import Data.String
import Data.Text

newtype User
  = User Text
  deriving (Show, Eq, Ord)

data WithUser a = WithUser User a deriving (Show, Eq)

withUser :: Text -> a -> WithUser a
withUser = WithUser . User

withoutUser :: WithUser a -> a
withoutUser (WithUser _ a) = a

instance IsString User where
  fromString = User . fromString

newtype FirstTime = FirstTime Bool
  deriving (Show, Eq)

isFirstTime :: FirstTime -> Bool
isFirstTime (FirstTime b) = b
