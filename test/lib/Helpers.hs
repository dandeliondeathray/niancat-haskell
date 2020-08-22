module Helpers where

import Application
import Service

import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test (SResponse)
import Service
import Test.Hspec
import Test.Hspec.Wai

testDictionary :: Dictionary
testDictionary =
  Dictionary $
    Map.fromList
      [ (key "VANTRIVAS", [word "VANTRIVAS"]),
        (key "PIKÉTRÖJA", [word "PIKÉTRÖJA"]),
        (key "DATORSPEL", [word "DATORSPEL", word "SPELDATOR", word "REPSOLDAT", word "LEDARPOST"])
      ]

withS :: NiancatState -> SpecWith (TVar NiancatState, Application) -> Spec
withS s = withState (buildNiancat testDictionary s)

assertS :: (NiancatState -> IO ()) -> WaiSession (TVar NiancatState) ()
assertS assertion = do
  st <- getState
  liftIO $ do
    s <- readTVarIO st
    assertion s

sendJson :: Method -> B.ByteString -> LB.ByteString -> WaiSession st SResponse
sendJson method path = request method path [(hContentType, "application/json")]

putJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
putJson = sendJson methodPut
postJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse

postJson = sendJson methodPost
