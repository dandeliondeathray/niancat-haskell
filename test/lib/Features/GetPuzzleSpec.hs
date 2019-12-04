{-# LANGUAGE QuasiQuotes #-}

module Features.GetPuzzleSpec where

import           Application
import           Service
import           Web

import           Helpers

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Network.Wai                    (Application)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Hspec.Wai.QuickCheck
import           Test.QuickCheck.Instances.Text


emptyState = def :: NiancatState

spec = do
  describe "in an initial state" $ withS emptyState $ describe "GET /v2/puzzle" $ it "returns a message that Nian is not yet set" $ get "/v2/puzzle" `shouldRespondWith` [json|[{response_type: "reply", message: "Nian är inte satt."}]|]
  describe "with a puzzle set" $ withS (emptyState {currentPuzzle = Just (puzzle "TRÖJAPIKÉ")}) $ describe "GET /v2/puzzle" $ it "returns the puzzle" $ get "/v2/puzzle" `shouldRespondWith` [json|[{response_type:"reply", message: "TRÖ JAP IKE"}]|]
