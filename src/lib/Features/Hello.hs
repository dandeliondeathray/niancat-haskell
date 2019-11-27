{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Features.Hello where

import           Application
import           Web

import           Data.Maybe
import           Data.Text
import           Servant

newtype Greeting = Greeting Text

instance Response Greeting where
  messages (Greeting who) = [Reply ("Hello, "<> who<> "!")]

greet :: Maybe Text -> NiancatState -> Greeting
greet who _ = Greeting $ fromMaybe "niancat" who

type HelloAPI = QueryParam "who" Text :> Get '[JSON] [Message]
type HelloServer = Maybe Text -> AppM [Message]

helloAPI :: Proxy HelloAPI
helloAPI = Proxy

hello :: Maybe Text -> AppM [Message]
hello = query . greet
