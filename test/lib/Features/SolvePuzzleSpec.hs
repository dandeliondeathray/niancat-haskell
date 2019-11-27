{-# LANGUAGE QuasiQuotes #-}

module Features.SolvePuzzleSpec where

import           Application
import           Web

import           Features.SolvePuzzle
import           Helpers
import           Matchers

import           Data.Default.Class
import           Data.Text
import           Network.Wai.Test
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

postSolution :: Text -> WaiSession st SResponse
postSolution s = postJson "v2/solutions" [json|{"user": "foobar", "solution": #{s}}|]

spec :: Spec
spec = do
  describe "in an initial state" $ withS def $ describe "submitting a solution" $ do
    it "respons with NotSet" $ postSolution "foobar" `shouldRespondWith` allOf [Reply "Nian är inte satt än!"]
    it "respons with status 200" $ postSolution "foobar" `shouldRespondWith` 200
  describe "with a puzzle set" $ withS def { puzzle = Just $ Puzzle "TRIVASVAN"} $ do
    describe "submitting an incorrect solution" $ context "with matching letters" $ do
        it "in canonical form" $ postSolution "SVANTRIVA" `shouldRespondWith` exactly [Reply "Ordet SVANTRIVA finns inte med i SAOL."]
        it "in other form" $ postSolution "svantriva" `shouldRespondWith` exactly [Reply "Ordet SVANTRIVA finns inte med i SAOL."]
    describe "submitting a correct solution" $ it "responds that the answer is correct" $ postSolution "VANTRIVAS" `shouldRespondWith` allOf [Reply "Ordet VANTRIVAS är korrekt!"]
