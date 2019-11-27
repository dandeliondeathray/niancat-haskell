{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Features.Hello where

import           Application
import           Web

import           Data.Text
import           Servant

newtype Hello = Hello Text

newtype Greeting = Greeting Text

instance FromHttpApiData Hello where
  parseQueryParam p = return $ Hello p

instance Response Greeting where
  messages (Greeting who) = [Reply ("Hello, "<> who<> "!")]

greet :: Maybe Hello -> AppM [Message]
greet (Just (Hello who)) = return [Reply ("Hello, " <> who <> "!")]
greet Nothing            = return [Reply "Hello, niancat!"]

type HelloAPI = QueryParam "who" Hello :> Get '[JSON] [Message]
type HelloServer = Maybe Hello -> AppM [Message]

helloAPI :: Proxy HelloAPI
helloAPI = Proxy
