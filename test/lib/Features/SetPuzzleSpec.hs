{-# LANGUAGE QuasiQuotes #-}

module Features.SetPuzzleSpec where

import           Application
import           Service
import           Web

import           Matchers

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Web.Scotty.Trans       (scottyAppT)

import           Network.Wai            (Application)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

emptyState = def :: NiancatState

app :: NiancatState -> IO Network.Wai.Application
app state = do
  sync <- newTVarIO state
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO niancat

spec = do
  describe "in an initial state" $ do
    with (app emptyState) $ do
      describe "PUT v2/puzzle" $ do
        it "replies OK!" $ do
          put "v2/puzzle" [json|{puzzle: "foobar"}|] `shouldRespondWith`
            allOf [Reply "OK!"]
        it "notifies channel of the new puzzle" $ do
          put "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ"}|] `shouldRespondWith`
            allOf [Notification "Dagens nia är **TRÖ JAP IKE**"]
  describe "with a puzzle set" $ do
    let state = State {puzzle = Just (Puzzle "TRÖJAPIKÉ")}
     in with (app state) $ do
          describe "PUT /v2/puzzle" $ do
            describe "for an equivalent puzzle" $ do
              it "matching exactly, replies 'same puzzle'" $ do
                put "v2/puzzle" [json|{puzzle: "TRÖJAPIKÉ"}|] `shouldRespondWith`
                  exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
              it "anagram of current puzzle, replies 'same puzzle'" $ do
                put "v2/puzzle" [json|{puzzle: "JATRÖPIKÉ"}|] `shouldRespondWith`
                  exactly [Reply "Nian är redan satt till TRÖ JAP IKE"]
            describe "for a new puzzle" $ do
              it "replies OK!" $ do
                put "v2/puzzle" [json|{puzzle: "TRIVASVAN"}|] `shouldRespondWith`
                  atLeastOneOf [Reply "OK!"]
