{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Service where

import Control.Monad.Reader
import Data.Default.Class
import Data.Functor
import Data.Maybe
import Errors
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Environment

import Features.GetPuzzle
import Features.SetPuzzle
import Features.SolvePuzzle

import Context
import Debug.Events
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Replies
import Web
import Persistence.InMemory hiding (events)

type NiancatAPI =
    "v2" :> "puzzle" :> Get '[JSON] [Message]
    :<|> "v2" :> "puzzle" :> ReqBody '[JSON] (WithUser SetPuzzle) :> Put '[JSON] [Message]
    :<|> "v2" :> "solutions" :> ReqBody '[JSON] (WithUser SubmitSolution) :> Post '[JSON] [Message]
    :<|> "v2" :> "debug" :> "events" :> Get '[JSON] [InMemoryEvent]

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: Dictionary -> Ctx -> Application
niancat dict s = server s niancatAPI features
 where
  features =
      query getPuzzle
      :<|> command . setPuzzle dict
      :<|> command . solvePuzzle dict
      :<|> debug events

nt :: Ctx -> AppM a -> Handler a
nt s x = runReaderT x s

server :: (HasServer a '[]) => Ctx -> Proxy a -> ServerT a AppM -> Application
server s p srv = errorsAsJson $ serve p $ hoistServer p (nt s) srv

getDictionary :: IO Dictionary
getDictionary =
  lookupEnv "DICTIONARY_FILE"
    >>= readFile
    . fromMaybe "saol.txt"
    <&> build
    . lines

buildNiancat :: Dictionary -> NiancatState -> IO (Ctx, Application)
buildNiancat dictionary initialState = do
  ctx <- initialize initialState
  return (ctx, niancat dictionary ctx)

runNiancat :: IO ()
runNiancat = do
  dictionary <- getDictionary
  (_, a) <- buildNiancat dictionary def
  putStrLn "Serving niancat on port 3000"
  run 3000 . logStdoutDev $ a
