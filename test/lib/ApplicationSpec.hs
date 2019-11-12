module ApplicationSpec where

import           Application
import           Service
import           Web

import           Matchers

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           TextShow
import           Web.Scotty.Trans       (scottyAppT)

import           Network.Wai            (Application)
import           Network.Wai.Test
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

emptyState = def :: NiancatState

app :: NiancatState -> IO Network.Wai.Application
app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO niancat

spec = do
  describe "in an initial state" $ do
    with (app emptyState) $ do
      describe "GET /" $ do
        it "returns a hello message" $ do
          get "/" `shouldRespondWith` exactly [Reply "Hello, niancat!"]
        it "with status 200" $ do get "/" `shouldRespondWith` 200
        it "customzies the hello message if a query parameter is supplied" $ do
          get "/?who=cool cat" `shouldRespondWith`
            exactly [Reply "Hello, cool cat!"]
