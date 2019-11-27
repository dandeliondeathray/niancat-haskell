{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

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

-- feature :: HasServer a '[] => TVar NiancatState -> Proxy a -> ServerT a AppM -> Application
-- feature s proxy server = serve proxy $ hoistServer proxy (nt s) server

-- type Resolver a b = a -> NiancatState -> b

-- type Applyer a b = a -> NiancatState -> (NiancatState, b)

-- class FromRequest a where
--   parse :: Parser a

-- param :: (Parsable a) => Text -> ActionT Text WebM (Maybe a)
-- param name =
--   (Web.Scotty.Trans.param name >>= return . Just) `rescue` \_ -> return Nothing

-- query ::
--      (Response b, Query a b)
--   => StdMethod
--   -> RoutePattern
--   -> Parser a
--   -> Resolver a b
--   -> Handler
-- query method route parse resolve = do
--   addroute method route $ do
--     q <- parse
--     s <- getState
--     let r = resolve q s
--     json $ messages r

-- command ::
--      (Response b, Command a b)
--   => StdMethod
--   -> RoutePattern
--   -> Parser a
--   -> Applyer a b
--   -> Handler
-- command method route parse apply = do
--   addroute method route $ do
--     c <- parse
--     s <- getState
--     let (s', r) = apply c s
--     webM $ updateState s'
--     json $ messages r
