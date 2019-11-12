{-# LANGUAGE QuasiQuotes #-}

module Features.GetPuzzleSpec where

import           Application
import           Service
import           Web

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Network.Wai                    (Application)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Hspec.Wai.QuickCheck
import           Test.QuickCheck.Instances.Text
import           Web.Scotty.Trans               (scottyAppT)

app :: NiancatState -> IO Network.Wai.Application
app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO niancat

emptyState = def :: NiancatState

spec = do
  describe "in an initial state" $ do
    with (app emptyState) $ do
      describe "GET /v2/puzzle" $ do
        it "returns a message that Nian is not yet set" $ do
          get "/v2/puzzle" `shouldRespondWith`
            [json|[{response_type: "reply", message: "Nian är inte satt."}]|]
  describe "with a puzzle set" $ do
    let state = State {puzzle = Just (Puzzle "TRÖJAPIKÉ")}
    with (app state) $ do
      describe "GET /v2/puzzle" $ do
        it "returns the puzzle" $ do
          get "/v2/puzzle" `shouldRespondWith`
            [json|[{response_type:"reply", message: "TRÖ JAP IKE"}]|]
