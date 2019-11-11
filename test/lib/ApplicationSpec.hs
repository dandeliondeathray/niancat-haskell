{-# LANGUAGE QuasiQuotes #-}

module ApplicationSpec where

import           Application
import           Puzzle
import           Service
import           Web

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Data.Text.Lazy
import           Web.Scotty.Trans               (scottyAppT)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Hspec.Wai.QuickCheck
import           Test.QuickCheck.Instances.Text

emptyState = def :: NiancatState

app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO niancat

spec = do
  describe "in an initial state" $ do
    with (app emptyState) $ do
      describe "GET /" $ do
        it "returns a hello message" $ do
          get "/" `shouldRespondWith`
            [json|[{response_type:"reply",message:"Hello, niancat!"}]|]
        it "with status 200" $ do get "/" `shouldRespondWith` 200
        it "customzies the hello message if a query parameter is supplied" $ do
          get "/?who=cool cat" `shouldRespondWith`
            [json|[{response_type:"reply",message:"Hello, cool cat!"}]|]
      describe "GET /v2/puzzle" $ do
        it "returns a message that Nian is not yet set" $ do
          get "/v2/puzzle" `shouldRespondWith`
            [json|[{response_type: "reply", message: "Nian är inte satt."}]|]
  describe "with a puzzle set" $ do
    let state = State {puzzle = Just (Puzzle "TRÖJAPIKÉ")}
     in with (app state) $ do
          it "returns the puzzle when asked" $ do
            get "/v2/puzzle" `shouldRespondWith`
              [json|[{response_type:"reply", message: "TRÖ JAP IKE"}]|]
