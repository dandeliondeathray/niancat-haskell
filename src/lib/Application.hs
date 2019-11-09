module Application where

import           Data.Aeson
import           Data.Text
import           Web.Scotty     hiding (text)

import           Features.Hello
import           Puzzle

data User =
  User Text
  deriving (Show, Eq)

data State =
  State
    { puzzle :: Maybe Puzzle
    }

data SubmitUnsolution =
  SubmitUnsolution User Text
  deriving (Show, Eq)

data Message
  = Notification Text
  | Reply Text
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (Notification text) =
    object ["response_type" .= ("notification" :: Text), "message" .= text]
  toJSON (Reply text) =
    object ["response_type" .= ("reply" :: Text), "message" .= text]

niancat :: ScottyM ()
niancat = do
  hello
