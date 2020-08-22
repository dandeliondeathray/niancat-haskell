module Web where

import Application
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson
import Data.Text hiding (foldl)
import Servant

type AppM = ReaderT (TVar NiancatState) Handler

class Event e where
  apply :: NiancatState -> e -> NiancatState

class Response e where
  messages :: e -> [Message]

data Message
  = Notification Text
  | Reply Text
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (Notification text) =
    object ["response_type" .= ("notification" :: Text), "message" .= text]
  toJSON (Reply text) =
    object ["response_type" .= ("reply" :: Text), "message" .= text]

query :: Response r => (NiancatState -> r) -> AppM [Message]
query resolver = do
  ts <- ask
  s <- liftIO $ readTVarIO ts
  return . messages $ resolver s

command :: (Response e, Event e) => (NiancatState -> [e]) -> AppM [Message]
command resolver = do
  ts <- ask
  liftIO . atomically $ do
    s <- readTVar ts
    let es = resolver s
    let s' = evolve s es
    when (s' /= s) (writeTVar ts s')
    return $ es >>= messages

evolve :: Event e => NiancatState -> [e] -> NiancatState
evolve s es = foldl apply s es
