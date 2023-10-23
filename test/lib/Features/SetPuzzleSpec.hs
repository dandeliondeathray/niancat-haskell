{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Features.SetPuzzleSpec where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Features.SetPuzzle
import Niancat.Domain
import Niancat.Replies
import Niancat.Puzzle

import Helpers
import Matchers

emptyState = def :: NiancatState

spec = do
  describe "in an initial state" $ do
    describe "setting the puzzle" $ do
      let s = emptyState
      let p = puzzle "TRÖJAPIKÉ"
      let cmd = SetPuzzle p
      let es = setPuzzle testDictionary cmd s
      let s' = foldl apply s es
      it "updates the puzzle" $ currentPuzzle s' `shouldBe` Just p
      it "responds with PuzzleSet" $ es `shouldBe` [PuzzleSet p]
    withS emptyState $
      describe "PUT v2/puzzle" $ do
        it "replies OK!" $ putJson "v2/puzzle" [json|{puzzle: "foobar"}|] `shouldRespondWith` allOf [Reply "OK!"]
        it "notifies channel of the new puzzle" $ putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ"}|] `shouldRespondWith` allOf [Notification "Dagens nia är **TRÖ JAP IKE**"]
        it "stores the new puzzle" $ do
          putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ"}|] `shouldRespondWith` allOf [Notification "Dagens nia är **TRÖ JAP IKE**"]
          st <- getState
          liftIO $ do
            s <- readTVarIO st
            currentPuzzle s `shouldBe` Just (puzzle "TRÖJAPIKE")
  describe "with a puzzle set" $ do
    let p = puzzle "TRÖJAPIKÉ"
    let state = def {currentPuzzle = Just p}
    describe "setting an equivalent puzzle" $ do
      let p' = puzzle "JATRÖPIKÉ"
      let es = setPuzzle testDictionary (SetPuzzle p') state
      let s' = foldl apply state es
      it "does not change the puzzle" $ currentPuzzle s' `shouldBe` Just p
      it "replies with SamePuzzle" $ es `shouldBe` [SamePuzzleSet p]
      withS state $
        describe "PUT /v2/puzzle" $ do
          describe "for an equivalent puzzle" $ do
            it "matching exactly, replies 'same puzzle'" $
              putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ"}|]
                `shouldRespondWith` exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
            it "anagram of current puzzle, replies 'same puzzle'" $
              putJson "v2/puzzle" [json|{puzzle: "JATRÖPIKÉ"}|]
                `shouldRespondWith` exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
          describe "for a new puzzle" $ it "replies OK!" $ putJson "v2/puzzle" [json|{puzzle: "TRIVASVAN"}|] `shouldRespondWith` atLeastOneOf [Reply "OK!"]
