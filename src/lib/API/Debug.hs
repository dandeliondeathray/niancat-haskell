{-# LANGUAGE DataKinds #-}

module API.Debug where

import Context
import Control.Monad.Reader
import Data.Aeson
import Persistence.Events
import Servant
import Web

type API = Get '[JSON] [EventWithMeta]

api :: (Store s) => ServerT API (AppM s)
api = debug events

debug :: (Store s, ToJSON a) => (Ctx s -> IO a) -> AppM s a
debug resolver = ask >>= liftIO . resolver

events :: (Store s) => Ctx s -> IO [EventWithMeta]
events = getAll . store
