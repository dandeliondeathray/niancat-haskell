{-# LANGUAGE QuasiQuotes #-}

module Features.StreaksSpec where

import Data.Map
import Data.Time
import Features.Streaks
import Helpers
import Matchers
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Persistence.Events
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = do
  let monday =
        UTCTime
          { utctDay = fromGregorian 2023 10 30,
            utctDayTime = secondsToDiffTime 32940 -- 09:09
          }
  let tuesday =
        UTCTime
          { utctDay = fromGregorian 2023 10 31,
            utctDayTime = secondsToDiffTime 32940 -- 09:09
          }

  let wednesday =
        UTCTime
          { utctDay = fromGregorian 2023 11 01,
            utctDayTime = secondsToDiffTime 32940 -- 09:09
          }

  describe "week days are correctly defined" $ do
    it "monday" $ dayOfWeek (utctDay monday) `shouldBe` Monday
    it "tuesday" $ dayOfWeek (utctDay tuesday) `shouldBe` Tuesday
    it "wednesdah" $ dayOfWeek (utctDay wednesday) `shouldBe` Wednesday

  describe "streaks" $ do
    it "empty events list yield no messages" $ do
      streaks [] `shouldBe` Streaks empty

    it "setting the first puzzle yields no messages" $ do
      streaks
        [ imbue (User "foobar") monday (PuzzleSet $ puzzle "abc")
        ]
        `shouldBe` Streaks empty

    it "setting puzzle with a few solvers lists all of them" $ do
      streaks
        [ imbue (User "foobar") monday (PuzzleSet $ puzzle "abc"),
          imbue (User "foobar") monday (CorrectSolutionSubmitted (Word "abc") (FirstTime True)),
          imbue (User "raboof") monday (CorrectSolutionSubmitted (Word "abc") (FirstTime True)),
          imbue (User "boofar") monday (CorrectSolutionSubmitted (Word "abc") (FirstTime True)),
          imbue (User "rafoob") monday (CorrectSolutionSubmitted (Word "abc") (FirstTime True)),
          imbue (User "foobar") tuesday (PuzzleSet $ puzzle "def"),
          imbue (User "rafoob") tuesday (CorrectSolutionSubmitted (Word "abc") (FirstTime True)),
          imbue (User "boofar") tuesday (CorrectSolutionSubmitted (Word "def") (FirstTime True)),
          imbue (User "raboof") tuesday (CorrectSolutionSubmitted (Word "def") (FirstTime True)),
          imbue (User "raboof") tuesday (CorrectSolutionSubmitted (Word "def") (FirstTime False)),
          imbue (User "raboof") tuesday (CorrectSolutionSubmitted (Word "fde") (FirstTime True)),
          imbue (User "foobar") wednesday (PuzzleSet $ puzzle "efg")
        ]
        `shouldBe` Streaks (fromList [(3, [User "raboof"]), (2, [User "boofar", User "rafoob"])])

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
