{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web where

import           Application

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Text.Lazy
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

getState :: WebM NiancatState
getState = ask >>= liftIO . readTVarIO >>= return

updateState :: NiancatState -> WebM ()
updateState s = ask >>= liftIO . atomically . flip modifyTVar' (const s)

type Parsed a = ActionT Text WebM a

class FromRequest a where
  parse :: Parsed a
