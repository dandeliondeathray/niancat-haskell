{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Service where

import qualified API.Debug as Debug
import qualified API.V2 as V2
import Context
import Data.Default.Class
import Data.Functor
import Data.Maybe
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Niancat.Dictionary
import Persistence.Events
import Servant
import System.Environment
import Web

type NiancatAPI = "v2" :> V2.API :<|> "debug" :> Debug.API

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: (Store s) => Dictionary -> Ctx s -> Application
niancat dict ctx = server ctx niancatAPI $ V2.api dict :<|> Debug.api

getDictionary :: IO Dictionary
getDictionary =
  lookupEnv "DICTIONARY_FILE"
    >>= readFile
      . fromMaybe "saol.txt"
    <&> build
      . lines

runNiancat :: IO ()
runNiancat = do
  dictionary <- getDictionary
  ctx <- initialize def
  let a = niancat dictionary ctx
  putStrLn "Serving niancat on port 3000"
  run 3000 . logStdoutDev $ a
