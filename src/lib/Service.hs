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
import           Network.Wai.Middleware.RequestLogger

import           Network.Wai.Handler.Warp
import           Servant

type NiancatAPI =
  HelloAPI
    :<|> GetPuzzleAPI
    :<|> SetPuzzleAPI
    :<|> SolvePuzzleAPI

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: TVar NiancatState -> Application
niancat s = serve niancatAPI $ hoistServer niancatAPI (nt s) features
  where
    features = hello
      :<|> getPuzzle
      :<|> setPuzzle
      :<|> command . solvePuzzle

nt :: TVar NiancatState -> AppM a -> Handler a
nt s x = runReaderT x s

server :: HasServer a '[] => TVar NiancatState -> Proxy a -> ServerT a AppM -> Application
server s p srv = serve p $ hoistServer p (nt s) srv

runNiancat :: IO ()
runNiancat = do
  putStrLn "Serving niancat on port 3000"
  st <- newTVarIO def
  run 3000 $ logStdoutDev $ niancat st
