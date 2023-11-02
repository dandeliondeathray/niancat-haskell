{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Web where

import Context
import Control.Concurrent.STM
import Control.Monad.Reader
import Errors
import Niancat.Domain
import Niancat.Events
import Niancat.Replies
import Niancat.State
import Persistence.Events
import Servant

type AppM s = ReaderT (Ctx s) Handler

query :: (Store s) => (Response r) => (NiancatState -> r) -> AppM s [Message]
query resolver = do
  ts <- asks state
  s <- liftIO $ readTVarIO ts
  return $ messages . resolver $ s

project :: (Store s, Response r) => ([EventWithMeta] -> r) -> AppM s [Message]
project resolver = do
  s <- asks store
  es <- liftIO $ getAll s
  return $ messages . resolver $ es

command :: (Store s) => (NiancatState -> WithUser [NiancatEvent]) -> AppM s [Message]
command resolver = do
  ts <- asks state
  st <- asks store
  clock' <- asks clock
  liftIO $ do
    now <- clock'
    (u, es) <- atomically $ do
      s <- readTVar ts
      let WithUser u es = resolver s
      let s' = foldl apply s (fmap (WithUser u) es)
      when (s' /= s) (writeTVar ts s')
      return (u, es)
    append (fmap (imbue u now) es) st
    return $ es >>= messages

server :: (HasServer a '[], Store s) => Ctx s -> Proxy a -> ServerT a (AppM s) -> Application
server ctx p srv = errorsAsJson $ serve p $ hoistServer p (nt ctx) srv

nt :: (Store s) => Ctx s -> AppM s a -> Handler a
nt ctx s = runReaderT s ctx
