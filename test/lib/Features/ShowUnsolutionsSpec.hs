module Features.ShowUnsolutionsSpec where

import Data.Default.Class
import Data.Time
import Data.Time.DSL
import Niancat.Domain
import Niancat.Events
import Niancat.Puzzle
import Niancat.Replies
import Persistence.Events
import Test.Helpers
import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Matchers

spec :: Spec
spec = do
  let withEvents = withContext getCurrentTime def
  describe "showing unsolutions for a specific user" $ do
    context "when no unsolutions have been posted" $ do
      withEvents [] $ do
        it "responds with a default message" $ do
          get "v2/unsolutions/foo" `shouldRespondWith` allOf [Reply "Inga olösningar satta."]

    context "when some unsolutions exists" $ do
      withEvents
        [ Imbued (PuzzleSet $ puzzle "1") (Meta (User "foo") (adv 0 monday)),
          -- this one should not be in the list, since there is a new puzzle set after it
          Imbued (UnsolutionSaved "qux") (Meta (User "foo") (adv 1 monday)),
          Imbued (PuzzleSet $ puzzle "2") (Meta (User "") (adv 0 tuesday)),
          Imbued (UnsolutionSaved "foo") (Meta (User "foo") (adv 1 tuesday)),
          Imbued (UnsolutionSaved "bar") (Meta (User "foo") (adv 2 tuesday)),
          Imbued (UnsolutionSaved "baz") (Meta (User "bar") (adv 3 tuesday))
        ]
        $ do
          it "responds with only this users unsolutions" $ do
            get "v2/unsolutions/foo" `shouldRespondWith` exactly [Reply "* foo\n* bar"]
            get "v2/unsolutions/bar" `shouldRespondWith` exactly [Reply "* baz"]

          it "does not show any unsolutions if only others have posted" $ do
            get "v2/unsolutions/baz" `shouldRespondWith` exactly [Reply "Inga olösningar satta."]

  describe "showing unsolutions for everybody" $ do
    context "with some unsolutions stored"
      $ withEvents
        [ Imbued (PuzzleSet $ puzzle "1") (Meta (User "foo") (adv 0 monday)),
          -- this one should not be in the list, since there is a new puzzle set after it
          Imbued (UnsolutionSaved "qux") (Meta (User "foo") (adv 1 monday)),
          Imbued (PuzzleSet $ puzzle "2") (Meta (User "") (adv 0 tuesday)),
          Imbued (UnsolutionSaved "foo") (Meta (User "foo") (adv 1 tuesday)),
          Imbued (UnsolutionSaved "bar") (Meta (User "foo") (adv 2 tuesday)),
          Imbued (UnsolutionSaved "baz") (Meta (User "bar") (adv 3 tuesday))
        ]
      $ do
        it "responds with unsolutions for everybody" $ do
          get "v2/unsolutions"
            `shouldRespondWith` exactly
              [ Reply "bar:\n* baz",
                Reply "foo:\n* foo\n* bar"
              ]
