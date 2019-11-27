module ApplicationSpec where

import           Application
import           Service
import           Web

import           Matchers

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           TextShow

import           Helpers

import           Network.Wai
import           Network.Wai.Test
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

emptyState = def :: NiancatState

spec = describe "in an initial state" $ withS emptyState $ describe "GET /" $ do
  it "returns a hello message" $ get "/" `shouldRespondWith` exactly [Reply "Hello, niancat!"]
  it "with status 200" $ get "/" `shouldRespondWith` 200
  it "customzies the hello message if a query parameter is supplied" $ get "/?who=cool cat" `shouldRespondWith` exactly [Reply "Hello, cool cat!"]
