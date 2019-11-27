module Helpers where

import           Application
import           Service

import           Control.Concurrent.STM
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Test       (SResponse)
import           Test.Hspec
import           Test.Hspec.Wai

wrapp :: NiancatState -> IO (TVar NiancatState, Application)
wrapp state = do
    s <- newTVarIO state
    let a = niancat s
    return (s, a)

withS :: NiancatState -> SpecWith (TVar NiancatState, Application) -> Spec
withS s = withState (wrapp s)

sendJson :: Method ->  B.ByteString -> LB.ByteString -> WaiSession st SResponse
sendJson method path = request method path [(hContentType, "application/json")]

putJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
putJson = sendJson methodPut
postJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
postJson = sendJson methodPost
