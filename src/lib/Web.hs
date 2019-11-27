module Web where

import           Application

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Servant

type AppM = ReaderT (TVar NiancatState) Handler

getState :: ReaderT (TVar a) Handler a
getState = do
    s <- ask
    liftIO $ readTVarIO s

modifyState :: (a -> a) -> ReaderT (TVar a) Handler ()
modifyState f = do
    st <- ask
    liftIO . atomically $ readTVar st >>= writeTVar st . f

query :: Response r => (s -> r) -> ReaderT (TVar s) Handler [Message]
query resolver = do
    ts <- ask
    s <- liftIO $ readTVarIO ts
    return . messages $ resolver s

command :: Response r => (s -> (s, r)) -> ReaderT (TVar s) Handler [Message]
command resolver = do
    ts <- ask
    liftIO . atomically $ do
        s <- readTVar ts
        let (s', r) = resolver s
        writeTVar ts s'
        return $ messages r
