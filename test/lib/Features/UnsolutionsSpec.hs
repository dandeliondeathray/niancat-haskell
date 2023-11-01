{-# LANGUAGE QuasiQuotes #-}

module Features.UnsolutionsSpec where

import Arbitrary ()
import Data.Default.Class
import Data.Map
import Data.Text
import Data.Time
import Helpers
import Matchers
import Network.Wai.Test
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Niancat.State
import Test.Hspec hiding (pending, pendingWith)
import Test.Hspec.Wai hiding (post)
import Test.Hspec.Wai.JSON

postUnsolution :: Text -> Text -> WaiSession st SResponse
postUnsolution u t = postJson "v2/unsolutions" [json|{"user": #{u}, "text": #{t}}|]

spec :: Spec
spec = do
  describe "storing unsolutions" $ do
    context "when no puzzle is set" $ do
      withContext getCurrentTime def [] $ do
        it "it returns a message saying as much" $ do
          postUnsolution "foo" "whatever" `shouldRespondWith` exactly [Reply "Nian är inte satt än!"]
    context "when a puzzle is set" $ do
      context "and no pending unsolution" $ do
        withContext getCurrentTime def {currentPuzzle = Just $ puzzle "FOO BAR BAZ"} [] $ do
          describe "with a string that contains a puzzle match" $ do
            it "emits a message about the solution being stored" $ do
              postUnsolution "foo" "ZAB-FOOBAR is a pretty funny word" `shouldRespondWith` exactly [Reply "Sparat."]
          describe "with a string that does not contain a puzzle match" $ do
            it "emits a message about trying again" $ do
              let expectedMessage = "Inget ord i olösningen matchar pusslet FOO BAR BAZ. Skriv !olösning för att bekräfta."
              postUnsolution "foo" "bar" `shouldRespondWith` exactly [Reply expectedMessage]
            it "stores the submitted solution as pending" $ do
              _ <- postUnsolution "foo" "bar"
              assertS $ \s -> pendingUnsolution (User "foo") s `shouldBe` Just "bar"
      context "and there is a pending unsolution" $ do
        let state = def {currentPuzzle = Just $ puzzle "FOO BAR BAZ", pendingUnsolutions = fromList [(User "foo", "a text without a fuzzy match for the puzzle")]}
        withContext getCurrentTime state [] $ do
          describe "when posting !olösning" $ do
            it "emits a stored event" $ do
              postUnsolution "foo" "" `shouldRespondWith` exactly [Reply "Sparat."]
            it "clears the pending unsolution" $ do
              _ <- postUnsolution "foo" ""
              assertS $ \s -> pendingUnsolution (User "foo") s `shouldBe` Nothing
          describe "with a string that contains a puzzle match" $ do
            let unsolution = "ZAB-FOOBAR is a pretty funny word"
            it "it emits a stored event" $ do
              postUnsolution "foo" unsolution `shouldRespondWith` exactly [Reply "Sparat."]
            it "clears the pending unsolution" $ do
              _ <- postUnsolution "foo" unsolution
              assertS $ \s -> pendingUnsolution (User "foo") s `shouldBe` Nothing
