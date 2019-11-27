{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Features.Hello where

import           Application
import           Web

import           Control.Concurrent.STM
import           Data.Text
import           Servant

newtype Hello = Hello Text

newtype Greeting = Greeting Text

instance FromHttpApiData Hello where
  parseQueryParam p = return $ Hello p

instance Query Hello Greeting where
  resolve (Hello who) _ = Greeting who

instance Response Greeting where
  messages (Greeting who) = [Reply ("Hello, "<> who<> "!")]

greet :: Maybe Hello -> AppM [Message]
greet (Just (Hello who)) = return [Reply ("Hello, " <> who <> "!")]
greet Nothing            = return [Reply "Hello, niancat!"]

type HelloAPI = QueryParam "who" Hello :> Get '[JSON] [Message]
type HelloServer = Maybe Hello -> AppM [Message]

helloAPI :: Proxy HelloAPI
helloAPI = Proxy

-- hello :: TVar NiancatState -> Application
-- hello s = feature s helloAPI greet
