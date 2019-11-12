module Features.Unsolutions where

import           Data.Aeson
import           Data.Text.Lazy

import           Application

data SubmitUnsolution =
  SubmitUnsolution User Text
  deriving (Show, Eq)

instance FromJSON (Text -> SubmitUnsolution) where
  parseJSON =
    withObject "unsolution" $ \v -> do
      txt <- v .: "text"
      return $ \user -> SubmitUnsolution (User user) txt
