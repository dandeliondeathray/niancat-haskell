module Features.GetPuzzleSpec where

import Data.Default.Class
import Helpers
import Matchers
import Niancat.Puzzle
import Niancat.Replies
import Niancat.State
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.QuickCheck ()
import Test.QuickCheck.Instances.Text ()

emptyState :: NiancatState
emptyState = def :: NiancatState

spec :: Spec
spec = do
  describe "in an initial state" $ do
    withS emptyState $ do
      describe "GET /v2/puzzle" $ do
        it "returns a message that Nian is not yet set" $ do
          get "/v2/puzzle" `shouldRespondWith` allOf [Reply "Nian är inte satt."]

  describe "with a puzzle set" $ do
    withS (emptyState {currentPuzzle = Just (puzzle "TRÖJAPIKÉ")}) $ do
      describe "GET /v2/puzzle" $ do
        it "returns the puzzle" $ do
          get "/v2/puzzle" `shouldRespondWith` allOf [Reply "TRÖ JAP IKE"]
