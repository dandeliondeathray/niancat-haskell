{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Web
  ( Web.param
  , jsonData
  , query
  , command
  , FromRequest(parse)
  , Handler
  , Parser
  , Resolver
  , WebM(runWebM)
  , module Network.HTTP.Types.Method
  ) where

import           Application

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Text.Lazy
import           Network.HTTP.Types.Method
import           Web.Scotty.Trans

newtype WebM a =
  WebM
    { runWebM :: ReaderT (TVar NiancatState) IO a
    }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader (TVar NiancatState)
           )

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

getState :: ActionT Text WebM NiancatState
getState = webM $ ask >>= liftIO . readTVarIO >>= return

updateState :: NiancatState -> WebM ()
updateState s = ask >>= liftIO . atomically . flip modifyTVar' (const s)

type Handler = ScottyT Text WebM ()

type Parser a = ActionT Text WebM a

type Resolver a b = a -> NiancatState -> b

type Applyer a b = a -> NiancatState -> (NiancatState, b)

class FromRequest a where
  parse :: Parser a

param :: (Parsable a) => Text -> ActionT Text WebM (Maybe a)
param name =
  (Web.Scotty.Trans.param name >>= return . Just) `rescue` \_ -> return Nothing

query ::
     (Response b, Query a b)
  => StdMethod
  -> RoutePattern
  -> Parser a
  -> Resolver a b
  -> Handler
query method route parse resolve = do
  addroute method route $ do
    q <- parse
    s <- getState
    let r = resolve q s
    json $ messages r s

command ::
     (Response b, Command a b)
  => StdMethod
  -> RoutePattern
  -> Parser a
  -> Applyer a b
  -> Handler
command method route parse apply = do
  addroute method route $ do
    c <- parse
    s <- getState
    let (s', r) = apply c s
    webM $ updateState s'
    json $ messages r s'
