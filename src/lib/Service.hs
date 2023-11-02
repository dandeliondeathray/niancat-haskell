{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Service where

import qualified API.Debug as Debug
import qualified API.V1 as V1
import qualified API.V2 as V2
import Context
import Data.Functor
import Data.Maybe
import Niancat.Dictionary
import Persistence.Events
import Servant
import System.Environment
import Web

type NiancatAPI = "v1" :> V1.API :<|> "v2" :> V2.API :<|> "debug" :> Debug.API

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: (Store s) => Dictionary -> Ctx s -> Application
niancat dict ctx = server ctx niancatAPI $ V1.api dict :<|> V2.api dict :<|> Debug.api

getDictionary :: IO Dictionary
getDictionary =
  lookupEnv "DICTIONARY_FILE"
    >>= readFile
      . fromMaybe "saol.txt"
    <&> build
      . lines
