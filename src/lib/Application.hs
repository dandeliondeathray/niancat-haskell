{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Application
  ( module Application
  , module Puzzle
  ) where

import           Data.Aeson
import           Data.Default.Class
import           Data.Text.Lazy
import           GHC.Generics
import           TextShow
import           TextShow.Generic

import           Puzzle

data User =
  User Text
  deriving (Generic, Eq)
  deriving TextShow via FromGeneric User

data NiancatState =
  State
    { puzzle :: Maybe Puzzle
    }

instance Default NiancatState where
  def = State {puzzle = Nothing}

class Response b =>
      Command a b
  where
  apply :: a -> NiancatState -> (NiancatState, b)

class Response b =>
      Query a b
  where
  resolve :: a -> NiancatState -> b

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
