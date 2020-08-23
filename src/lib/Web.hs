module Web where

import Control.Concurrent.STM
import Control.Monad.Reader
import Servant

import Niancat.Domain
import Niancat.Replies

type AppM = ReaderT (TVar NiancatState) Handler

query :: Response r => (NiancatState -> r) -> AppM [Message]
query resolver = do
  ts <- ask
  s <- liftIO $ readTVarIO ts
  return $ messages . resolver $ s

command :: (NiancatState -> [NiancatEvent]) -> AppM [Message]
command resolver = do
  ts <- ask
  liftIO $ do
    (_, es) <- atomically $ do
      s <- readTVar ts
      let es = resolver s
      let s' = foldl apply s es
      when (s' /= s) (writeTVar ts s')
      return (s', es)
    return $ es >>= messages
