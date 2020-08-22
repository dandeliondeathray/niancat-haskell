{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Service where

import Application
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Data.Functor
import Data.Maybe
import Errors
import Features.GetPuzzle
import Features.Hello
import Features.SetPuzzle
import Features.SolvePuzzle
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Environment
import Web

type NiancatAPI =
  HelloAPI
    :<|> "v2" :> "puzzle" :> Get '[JSON] [Message]
    :<|> "v2" :> "puzzle" :> ReqBody '[JSON] SetPuzzle :> Put '[JSON] [Message]
    :<|> "v2" :> "solutions" :> ReqBody '[JSON] SubmitSolution :> Post '[JSON] [Message]

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: Dictionary -> TVar NiancatState -> Application
niancat dict s = server s niancatAPI features
  where
    features =
      hello
        :<|> query getPuzzle
        :<|> command . setPuzzle dict
        :<|> command . solvePuzzle dict

nt :: TVar NiancatState -> AppM a -> Handler a
nt s x = runReaderT x s

server :: HasServer a '[] => TVar NiancatState -> Proxy a -> ServerT a AppM -> Application
server s p srv = errorsAsJson $ serve p $ hoistServer p (nt s) srv

getDictionary :: IO Dictionary
getDictionary =
  lookupEnv "DICTIONARY_FILE"
    >>= readFile . fromMaybe "saol.txt"
    <&> build . lines

buildNiancat :: Dictionary -> NiancatState -> IO (TVar NiancatState, Application)
buildNiancat dictionary state = do
  s <- newTVarIO state
  let a = niancat dictionary s
  return (s, a)

runNiancat :: IO ()
runNiancat = do
  dictionary <- getDictionary
  (_, a) <- buildNiancat dictionary def
  putStrLn "Serving niancat on port 3000"
  run 3000 . logStdoutDev $ a
