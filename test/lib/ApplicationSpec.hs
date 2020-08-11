module ApplicationSpec where

import Application
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Helpers
import Matchers
import Network.Wai
import Network.Wai.Test
import Service
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import TextShow
import Web

emptyState = def :: NiancatState

spec = describe "in an initial state" $
  withS emptyState $
    describe "GET /" $ do
      it "returns a hello message" $ get "/" `shouldRespondWith` exactly [Reply "Hello, niancat!"]
      it "with status 200" $ get "/" `shouldRespondWith` 200
      it "customzies the hello message if a query parameter is supplied" $ get "/?who=cool cat" `shouldRespondWith` exactly [Reply "Hello, cool cat!"]
