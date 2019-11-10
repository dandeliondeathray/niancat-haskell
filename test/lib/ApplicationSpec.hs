{-# LANGUAGE QuasiQuotes #-}

module ApplicationSpec where

import           Application
import           Service
import           Web

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Web.Scotty.Trans       (scottyAppT)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

emptyState = def :: NiancatState

app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO niancat

spec =
  with (app emptyState) $ do
    describe "GET /" $ do
      it "returns a hello message" $ do
        get "/" `shouldRespondWith`
          [json|[{response_type:"reply",message:"Hello, niancat!"}]|]
      it "with status 200" $ do get "/" `shouldRespondWith` 200
      it "customzies the hello message if a query parameter is supplied" $ do
        get "/?who=cool cat" `shouldRespondWith`
          [json|[{response_type:"reply",message:"Hello, cool cat!"}]|]
