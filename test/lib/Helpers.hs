module Helpers where

import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai

import Context
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle
import Persistence.Events
import Persistence.InMemory
import Service

testDictionary :: Dictionary
testDictionary =
  Dictionary
    $ Map.fromList
      [ (key "VANTRIVAS", [Word "VANTRIVAS"])
      , (key "PIKÉTRÖJA", [Word "PIKÉTRÖJA"])
      , (key "DATORSPEL", [Word "DATORSPEL", Word "SPELDATOR", Word "REPSOLDAT", Word "LEDARPOST"])
      ]

withS :: NiancatState -> SpecWith (Ctx InMemoryStore, Application) -> Spec
withS s = withState $ do
  s' <- newTVarIO s
  e' <- newInMemoryStore
  let ctx = Ctx{state = s', store = e'}

  return (ctx, niancat testDictionary ctx)

assertS :: (Store s) => (NiancatState -> IO ()) -> WaiSession (Ctx s) ()
assertS assertion = do
  st <- getState
  liftIO $ do
    s <- readTVarIO (state st)
    assertion s

sendJson :: Method -> B.ByteString -> LB.ByteString -> WaiSession st SResponse
sendJson method path = request method path [(hContentType, "application/json")]

putJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
putJson = sendJson methodPut
postJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
postJson = sendJson methodPost
