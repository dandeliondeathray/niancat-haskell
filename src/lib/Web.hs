{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Web where

import Context
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text hiding (append, foldl)
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

project1 :: (Store s, Response r) => (a -> [EventWithMeta] -> r) -> a -> AppM s [Message]
project1 resolver input = do
  s <- asks store
  es <- liftIO $ getAll s
  return $ messages . resolver input $ es

command :: (Store s) => (a -> NiancatState -> WithUser [NiancatEvent]) -> a -> AppM s [Message]
command resolver input = do
  ts <- asks state
  st <- asks store
  clock' <- asks clock
  liftIO $ do
    now <- clock'
    (u, es) <- atomically $ do
      s <- readTVar ts
      let WithUser u es = resolver input s
      let s' = foldl apply s (fmap (WithUser u) es)
      when (s' /= s) (writeTVar ts s')
      return (u, es)
    append (fmap (imbue u now) es) st
    return $ es >>= messages

withUserFromPath :: (Store s) => (WithUser a -> AppM s [Message]) -> (Text -> a -> AppM s [Message])
withUserFromPath request u i = request $ WithUser (User u) i

userFromPath :: (Store s) => (User -> AppM s [Message]) -> Text -> AppM s [Message]
userFromPath request u = request (User u)

server :: (HasServer a '[], Store s) => Ctx s -> Proxy a -> ServerT a (AppM s) -> Application
server ctx p srv = errorsAsJson $ serve p $ hoistServer p (nt ctx) srv

nt :: (Store s) => Ctx s -> AppM s a -> Handler a
nt ctx s = runReaderT s ctx

(><>>) :: (Store s, Monoid a) => (i -> AppM s a) -> AppM s a -> (i -> AppM s a)
first ><>> second = \input -> do
  first' <- first input
  second' <- second
  return (first' <> second')
