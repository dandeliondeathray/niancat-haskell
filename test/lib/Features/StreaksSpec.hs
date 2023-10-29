{-# LANGUAGE QuasiQuotes #-}

module Features.StreaksSpec where

import Arbitrary
import Data.Map
import Data.Time
import Features.Streaks
import Helpers
import Matchers
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Persistence.Events
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.QuickCheck.Monadic ()

spec :: Spec
spec = do
  describe "streaks" $ do
    describe "setting the puzzle" $ do
      context "on a weekday" $ do
        prop "makes the new puzzle count" $ \s u p (Weekday ts) -> do
          let s' = march s (imbue u ts (PuzzleSet p))
          counts s' `shouldBe` True

      context "on a weekend" $ do
        prop "makes the new puzzle not count" $ \s u p (Weekend ts) -> do
          let s' = march s (imbue u ts (PuzzleSet p))
          counts s' `shouldBe` False

      describe "when the current one does not count" $ do
        let states s u p ts = (before, after)
              where
                before = s {counts = False}
                after = march before (imbue u ts (PuzzleSet p))

        prop "does not affect current score" $ \s u p ts -> do
          let (before, after) = states s u p ts
          currentScore after `shouldBe` currentScore before

        prop "does not affect unbroken streaks" $ \s u p ts -> do
          let (before, after) = states s u p ts
          unbroken after `shouldBe` unbroken before

      describe "when the current one counts" $ do
        let states s u p ts = (before, after)
              where
                before = s {counts = True}
                after = march before (imbue u ts (PuzzleSet p))

        -- prop "bumps score for everyone who had solved the puzzle" $ \s u p ts -> do
        --   let (before, after) = states s u p ts
        --   True `shouldBe` False
        -- prop "resets the score for everyone who had not solved the puzzle" $ \s u p ts -> do
        --   let (before, after) = states s u p ts
        --   True `shouldBe` False

        prop "resets the current score counter" $ \s u p ts -> do
          let (_, after) = states s u p ts
          currentScore after `shouldBe` empty

  describe "PUT /v2/puzzle" $ do
    let pastEvents =
          withMeta
            <$> [ (PuzzleSet (puzzle "VANTRIVAS"), "foobar", monday),
                  (CorrectSolutionSubmitted (Word "VANTRIVAS") (FirstTime True), "foobar", tuesday)
                ]
          where
            withMeta (e, u, d) = Imbued e (Meta (User u) d)

    let currentState =
          State
            { currentPuzzle = Just $ puzzle "VANTRIVAS",
              solvers = singleton (Word "VANTRIVAS") [User "foobar"]
            }

    let clock = return wednesday :: IO UTCTime

    withContext clock currentState pastEvents $ do
      it "reports appropriate streak lengths" $ do
        putJson "/v2/puzzle" [json|{puzzle: "LEDARPOST", user: "boofar"}|] `shouldRespondWith` allOf [Notification "**Obrutna serier:**\n1: foobar"]
