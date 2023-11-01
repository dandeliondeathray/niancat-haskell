{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Service where

import Context
import Control.Monad.Reader
import Data.Default.Class
import Data.Functor
import Data.Maybe
import Debug.Events
import Errors
import Features.GetPuzzle
import Features.SetPuzzle
import Features.SolvePuzzle
import Features.Streaks
import Features.Unsolutions
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Replies
import Persistence.Events
import Servant
import System.Environment
import Web

type NiancatAPI =
  "v2" :> "puzzle" :> Get '[JSON] [Message]
    :<|> "v2" :> "puzzle" :> ReqBody '[JSON] (WithUser SetPuzzle) :> Put '[JSON] [Message]
    :<|> "v2" :> "solutions" :> ReqBody '[JSON] (WithUser SubmitSolution) :> Post '[JSON] [Message]
    :<|> "v2" :> "streaks" :> Get '[JSON] [Message]
    :<|> "v2" :> "unsolutions" :> ReqBody '[JSON] (WithUser SubmitUnsolution) :> Post '[JSON] [Message]
    :<|> "v2" :> "debug" :> "events" :> Get '[JSON] [EventWithMeta]

niancatAPI :: Proxy NiancatAPI
niancatAPI = Proxy

niancat :: (Store s) => Dictionary -> Ctx s -> Application
niancat dict s = server s niancatAPI features
  where
    features =
      query getPuzzle
        :<|> (\req -> (++) <$> command (setPuzzle dict req) <*> project streaks)
        :<|> command . solvePuzzle dict
        :<|> project streaks
        :<|> command . submitUnsolution
        :<|> debug events

nt :: (Store s) => Ctx s -> AppM s a -> Handler a
nt s x = runReaderT x s

server :: (HasServer a '[], Store s) => Ctx s -> Proxy a -> ServerT a (AppM s) -> Application
server s p srv = errorsAsJson $ serve p $ hoistServer p (nt s) srv

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
