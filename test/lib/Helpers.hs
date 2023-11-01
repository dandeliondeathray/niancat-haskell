module Helpers where

import Context
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test (SResponse)
import Niancat.Dictionary
import Niancat.Puzzle
import Niancat.State
import Persistence.Events
import Persistence.InMemory
import Service
import Test.Hspec
import Test.Hspec.Wai

testDictionary :: Dictionary
testDictionary =
  Dictionary $
    Map.fromList
      [ (key "VANTRIVAS", [Word "VANTRIVAS"]),
        (key "PIKÉTRÖJA", [Word "PIKÉTRÖJA"]),
        (key "DATORSPEL", [Word "DATORSPEL", Word "SPELDATOR", Word "REPSOLDAT", Word "LEDARPOST"])
      ]

withS :: NiancatState -> SpecWith (Ctx InMemoryStore, Application) -> Spec
withS s = withState $ do
  s' <- newTVarIO s
  e' <- newInMemoryStore
  let c = getCurrentTime
  let ctx = Ctx {state = s', store = e', clock = c}

  return (ctx, niancat testDictionary ctx)

withContext :: IO UTCTime -> NiancatState -> [EventWithMeta] -> SpecWith (Ctx InMemoryStore, Application) -> Spec
withContext c s es = withState $ do
  s' <- newTVarIO s
  e' <- newInMemoryStore
  liftIO $ append es e'
  let ctx = Ctx {state = s', store = e', clock = c}

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

monday :: UTCTime
monday =
  UTCTime
    { utctDay = fromGregorian 2023 10 30,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

tuesday :: UTCTime
tuesday =
  UTCTime
    { utctDay = fromGregorian 2023 10 31,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

wednesday :: UTCTime
wednesday =
  UTCTime
    { utctDay = fromGregorian 2023 11 01,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

thursday :: UTCTime
thursday =
  UTCTime
    { utctDay = fromGregorian 2023 11 02,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

friday :: UTCTime
friday =
  UTCTime
    { utctDay = fromGregorian 2023 11 03,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

saturday :: UTCTime
saturday =
  UTCTime
    { utctDay = fromGregorian 2023 11 04,
      utctDayTime = secondsToDiffTime 32940
    }

sunday :: UTCTime
sunday =
  UTCTime
    { utctDay = fromGregorian 2023 11 05,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }
