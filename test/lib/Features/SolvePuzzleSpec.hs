{-# LANGUAGE QuasiQuotes #-}

module Features.SolvePuzzleSpec where

import Data.Default.Class
import Data.Map
import Data.Text
import Network.Wai.Test
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Niancat.State
import Test.Helpers
import Test.Hspec
import Test.Hspec.Wai hiding (get)
import Test.Hspec.Wai.JSON
import Test.Matchers

postSolution :: Text -> Text -> WaiSession st SResponse
postSolution u s = postJson "v2/solutions" [json|{"user": #{u}, "solution": #{s}}|]

spec :: Spec
spec = do
  let user = "foobar"
  describe "in an initial state" $ do
    withS def $
      describe "submitting a solution" $
        do
          let act = postSolution user "VANTRIVAS"
          it "respons with NotSet" $ act `shouldRespondWith` allOf [Reply "Nian är inte satt än!"]
          it "respons with status 200" $ act `shouldRespondWith` 200
  describe "with a puzzle set" $
    withS def {currentPuzzle = Just $ puzzle "TRIVASVAN"} $
      do
        describe "submitting an incorrect solution" $
          context "with matching letters" $
            do
              it "in canonical form" $ postSolution user "SVANTRIVA" `shouldRespondWith` exactly [Reply "Ordet SVANTRIVA finns inte med i SAOL."]
              it "in other form" $ postSolution user "svantriva" `shouldRespondWith` exactly [Reply "Ordet SVANTRIVA finns inte med i SAOL."]
        describe "submitting a correct solution" $ do
          it "responds that the answer is correct" $
            postSolution user "VANTRIVAS"
              `shouldRespondWith` exactly [Reply "Ordet VANTRIVAS är korrekt!"]
          it "adds the user to today's solvers" $ do
            _ <- postSolution user "VANTRIVAS"
            assertS $ \s -> solvers s `shouldBe` fromList [(Word "VANTRIVAS", [User "foobar"])]
