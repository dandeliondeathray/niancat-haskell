{-# LANGUAGE QuasiQuotes #-}

module Features.SetPuzzleSpec where

import           Application
import           Features.SetPuzzle
import           Service
import           Web                    hiding (getState)

import           Helpers
import           Matchers

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Data.Text.Lazy

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

emptyState = def :: NiancatState

spec = do
  describe "in an initial state" $ do
    describe "setting the puzzle" $ do
      let s = emptyState
      let p = puzzle "TRÖJAPIKÉ"
      let cmd = SetPuzzle p
      let (s', r) = setPuzzle testDictionary cmd s
      it "updates the puzzle" $ currentPuzzle s' `shouldBe` Just p
      it "responds with PuzzleSet" $ r `shouldBe` PuzzleSet p
    withS emptyState $ describe "PUT v2/puzzle" $ do
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
    let state = State {currentPuzzle = Just p}
    describe "setting an equivalent puzzle" $ do
      let p' = puzzle "JATRÖPIKÉ"
      let (s', r) = setPuzzle testDictionary (SetPuzzle p) state
      it "does not change the puzzle" $ currentPuzzle s' `shouldBe` Just p
      it "replies with SamePuzzle" $ r `shouldBe` SamePuzzle p
      withS state $ describe "PUT /v2/puzzle" $ do
        describe "for an equivalent puzzle" $ do
          it "matching exactly, replies 'same puzzle'" $ putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ"}|] `shouldRespondWith`
              exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
          it "anagram of current puzzle, replies 'same puzzle'" $ putJson "v2/puzzle" [json|{puzzle: "JATRÖPIKÉ"}|] `shouldRespondWith`
              exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
        describe "for a new puzzle" $ it "replies OK!" $ putJson "v2/puzzle" [json|{puzzle: "TRIVASVAN"}|] `shouldRespondWith` atLeastOneOf [Reply "OK!"]
