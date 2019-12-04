{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Service where

import           Application
import           Features.GetPuzzle
import           Features.Hello
import           Features.SetPuzzle
import           Features.SolvePuzzle
import           Web

import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.Default.Class
import qualified Data.Map                             as Map
import           Network.Wai.Middleware.RequestLogger

import           Network.Wai.Handler.Warp
import           Servant

type NiancatAPI =
  HelloAPI
    :<|> "v2" :> "puzzle" :> Get '[JSON] [Message]
    :<|> "v2" :> "puzzle" :> ReqBody '[JSON] SetPuzzle :> Put '[JSON] [Message]
    :<|> "v2" :> "solutions" :> ReqBody '[JSON] SubmitSolution :> Post '[JSON] [Message]

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: Dictionary -> TVar NiancatState -> Application
niancat dict s = serve niancatAPI $ hoistServer niancatAPI (nt s) features
  where
    features = hello
      :<|> query getPuzzle
      :<|> command . setPuzzle
      :<|> command . solvePuzzle dict

nt :: TVar NiancatState -> AppM a -> Handler a
nt s x = runReaderT x s

server :: HasServer a '[] => TVar NiancatState -> Proxy a -> ServerT a AppM -> Application
server s p srv = serve p $ hoistServer p (nt s) srv

runNiancat :: IO ()
runNiancat = do
  putStrLn "Serving niancat on port 3000"
  let dict = Dictionary Map.empty -- TODO: initialize dictionary from file
  st <- newTVarIO def
  run 3000 $ logStdoutDev $ niancat dict st
