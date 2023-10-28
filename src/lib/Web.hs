{-# LANGUAGE TupleSections #-}
module Web where

import Data.Aeson
import Control.Concurrent.STM
import Control.Monad.Reader
import Servant

import Niancat.Domain
import Niancat.Replies
import Persistence.Events
import Context
import Data.Time (getCurrentTime)

type AppM = ReaderT Ctx Handler

query :: Response r => (NiancatState -> r) -> AppM [Message]
query resolver = do
  ts <- asks state
  s <- liftIO $ readTVarIO ts
  return $ messages . resolver $ s

type Resolver = (NiancatState -> WithUser [NiancatEvent])
command :: Resolver -> AppM [Message]
command resolver = do
  ts <- asks state
  st <- asks store
  liftIO $ do
    now <- getCurrentTime
    (u', es) <- atomically $ do
      s <- readTVar ts
      let WithUser (u, es) = resolver s
      let s' = foldl apply s (fmap (withUser u) es)
      when (s' /= s) (writeTVar ts s')
      return (u, es)
    append st now u' es
    return $ es >>= messages

debug :: ToJSON a => (Ctx -> IO a) -> AppM a
debug resolver = ask >>= liftIO . resolver
