{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Application where

import           Control.Monad.State
import           Data.Aeson
import           Data.Default.Class
import           Data.Text.Lazy

import           Puzzle

data User =
  User Text
  deriving (Show, Eq)

data NiancatState =
  State
    { puzzle :: Maybe Puzzle
    }

instance Default NiancatState where
  def = State {puzzle = Nothing}

class Command a where
  apply :: NiancatState -> a -> (NiancatState, [Message])

class Query a where
  resolve :: NiancatState -> a -> [Message]

acceptCommand :: Command c => c -> State NiancatState [Message]
acceptCommand c = do
  s <- get
  let (s', msgs) = apply s c
  put s'
  return msgs

acceptQuery :: Query q => q -> State NiancatState [Message]
acceptQuery q = do
  s <- get
  let msgs = resolve s q
  return msgs

data Message
  = Notification Text
  | Reply Text
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (Notification text) =
    object ["response_type" .= ("notification" :: Text), "message" .= text]
  toJSON (Reply text) =
    object ["response_type" .= ("reply" :: Text), "message" .= text]
