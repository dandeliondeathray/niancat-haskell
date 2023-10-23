{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Features.GetPuzzleSpec where

import Data.Default.Class
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.QuickCheck ()
import Test.QuickCheck.Instances.Text ()

import Niancat.Domain
import Niancat.Puzzle

import Helpers

emptyState = def :: NiancatState

spec = do
  describe "in an initial state" $ do
    withS emptyState $ do
      describe "GET /v2/puzzle" $ do
        it "returns a message that Nian is not yet set" $ do
          get "/v2/puzzle" `shouldRespondWith` [json|[{response_type: "reply", message: "Nian är inte satt."}]|]

  describe "with a puzzle set" $ do
    withS (emptyState {currentPuzzle = Just (puzzle "TRÖJAPIKÉ")}) $ do
      describe "GET /v2/puzzle" $ do
        it "returns the puzzle" $ do
          get "/v2/puzzle" `shouldRespondWith` [json|[{response_type:"reply", message: "TRÖ JAP IKE"}]|]
