{-# LANGUAGE QuasiQuotes #-}

module Features.SetPuzzleSpec where

import Context
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Features.Puzzle.Set
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Niancat.Replies
import Niancat.State
import Test.Helpers
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Matchers

spec :: Spec
spec = do
  describe "in an initial state" $ do
    describe "setting the puzzle" $ do
      let s = def
      let p = puzzle "TRÖJAPIKÉ"
      let cmd = withUser "foo" $ SetPuzzle p
      let es = setPuzzle testDictionary cmd s
      let s' = applyAll s es
      it "updates the puzzle" $ currentPuzzle s' `shouldBe` Just p
      it "responds with PuzzleSet" $ es `shouldBe` withUser "foo" [PuzzleSet p]
    withS def $
      describe "PUT v2/puzzle" $
        do
          it "replies OK!" $ putJson "v2/puzzle" [json|{puzzle: "JATRÖPIKÉ", user: "foo"}|] `shouldRespondWith` allOf [Reply "OK!"]
          it "notifies channel of the new puzzle" $ putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ", user: "foo"}|] `shouldRespondWith` allOf [Notification "Dagens nia är **TRÖ JAP IKE**"]
          it "stores the new puzzle" $ do
            putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ", user: "foo"}|] `shouldRespondWith` allOf [Notification "Dagens nia är **TRÖ JAP IKE**"]
            st <- getState
            liftIO $ do
              s <- readTVarIO . state $ st
              currentPuzzle s `shouldBe` Just (puzzle "TRÖJAPIKE")
  describe "with a puzzle set" $ do
    let p = puzzle "TRÖJAPIKÉ"
    let s = def {currentPuzzle = Just p}
    describe "setting an equivalent puzzle" $ do
      let p' = puzzle "JATRÖPIKÉ"
      let es = setPuzzle testDictionary (withUser "foo" $ SetPuzzle p') s
      let s' = applyAll s es
      it "does not change the puzzle" $ currentPuzzle s' `shouldBe` Just p
      it "replies with SamePuzzle" $ withoutUser es `shouldBe` [SamePuzzleSet p]
      withS s $
        describe "PUT /v2/puzzle" $
          do
            describe "for an equivalent puzzle" $ do
              it "matching exactly, replies 'same puzzle'" $
                putJson "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ", user:"foo"}|]
                  `shouldRespondWith` exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
              it "anagram of current puzzle, replies 'same puzzle'" $
                putJson "v2/puzzle" [json|{puzzle: "JATRÖPIKÉ", user:"foo"}|]
                  `shouldRespondWith` exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
            describe "for a new puzzle" $ it "replies OK!" $ putJson "v2/puzzle" [json|{puzzle: "TRIVASVAN", user:"foo"}|] `shouldRespondWith` atLeastOneOf [Reply "OK!"]
  describe "with invalid input" $ do
    let p = puzzle "FOOBARBAZ"
    let s = def

    let es = setPuzzle testDictionary (withUser "foo" $ SetPuzzle p) s
    let s' = applyAll s es

    it "does not change the puzzle" $ currentPuzzle s' `shouldBe` currentPuzzle s
    it "replies with invalid puzzle" $ withoutUser es `shouldBe` [InvalidPuzzleSet p]
