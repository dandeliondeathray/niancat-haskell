module Web where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson
import Servant

import Context
import Data.Time
import Niancat.Domain
import Niancat.Replies
import Persistence.Events

type AppM s = ReaderT (Ctx s) Handler

query :: (Store s) => (Response r) => (NiancatState -> r) -> AppM s [Message]
query resolver = do
  ts <- asks state
  s <- liftIO $ readTVarIO ts
  return $ messages . resolver $ s

type Resolver = (NiancatState -> WithUser [NiancatEvent])
command :: (Store s) => Resolver -> AppM s [Message]
command resolver = do
  ts <- asks state
  st <- asks store
  liftIO $ do
    now <- getCurrentTime
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
