module Application
  ( module Application
  , module Puzzle
  ) where

import           Data.Aeson
import           Data.Default.Class
import           Data.Text

import           Puzzle

newtype User =
  User Text
  deriving (Show, Eq)

newtype NiancatState = State{puzzle :: Maybe Puzzle} deriving (Show, Eq)

instance Default NiancatState where
  def = State {puzzle = Nothing}

class Response a where
  messages :: a -> [Message]

data Message
  = Notification Text
  | Reply Text
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (Notification text) =
    object ["response_type" .= ("notification" :: Text), "message" .= text]
  toJSON (Reply text) =
    object ["response_type" .= ("reply" :: Text), "message" .= text]
