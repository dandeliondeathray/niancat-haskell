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

import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle
import Context
import Service

testDictionary :: Dictionary
testDictionary =
  Dictionary $
    Map.fromList
      [ (key "VANTRIVAS", [Word "VANTRIVAS"]),
        (key "PIKÉTRÖJA", [Word "PIKÉTRÖJA"]),
        (key "DATORSPEL", [Word "DATORSPEL", Word "SPELDATOR", Word "REPSOLDAT", Word "LEDARPOST"])
      ]

withS :: NiancatState -> SpecWith (Ctx, Application) -> Spec
withS s = withState (buildNiancat testDictionary s)

assertS :: (NiancatState -> IO ()) -> WaiSession Ctx ()
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
