module Web where

import           Application

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text
import           Servant

type AppM = ReaderT (TVar NiancatState) Handler

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

query :: Response r => (NiancatState -> r) -> AppM [Message]
query resolver = do
    ts <- ask
    s <- liftIO $ readTVarIO ts
    return . messages $ resolver s

command :: Response r => (NiancatState -> (NiancatState, r)) -> AppM [Message]
command resolver = do
    ts <- ask
    liftIO . atomically $ do
        s <- readTVar ts
        let (s', r) = resolver s
        writeTVar ts s'
        return $ messages r
