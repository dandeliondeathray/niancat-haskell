module Web where

import Context
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson
import Niancat.Domain
import Niancat.Replies
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
      let WithUser (u, es) = resolver s
      let s' = foldl apply s (fmap (withUser u) es)
      when (s' /= s) (writeTVar ts s')
      return (u, es)
    append (fmap (imbue u now) es) st
    return $ es >>= messages

debug :: (Store s, ToJSON a) => (Ctx s -> IO a) -> AppM s a
debug resolver = ask >>= liftIO . resolver
