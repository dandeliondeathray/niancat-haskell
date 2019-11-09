module Features.Unsolutions where

import           Data.Aeson
import           Data.Text

import           Application

instance FromJSON (Text -> SubmitUnsolution) where
  parseJSON =
    withObject "unsolution" $ \v -> do
      txt <- v .: "text"
      return $ \user -> SubmitUnsolution (User user) txt
