{-# LANGUAGE ScopedTypeVariables #-}

module SerializationSpec where

import Arbitrary ()
import Data.Aeson
import Data.Text
import Features.Puzzle.Set
import Features.Puzzle.Solve
import Features.Unsolutions.Post
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

reply :: Text
reply = "reply"

notification :: Text
notification = "notification"

spec :: Spec
spec = do
  describe "Serialization of responses" $ do
    prop "Notification" $
      \message -> do
        let input = Notification message
            expected = encode $ object ["response_type" .= notification, "message" .= message]
         in encode input `shouldBe` expected

    prop "Reply" $ \message ->
      let input = Reply message
          expected = encode $ object ["response_type" .= reply, "message" .= message]
       in encode input `shouldBe` expected

  describe "Deserialization of commands" $ do
    prop "SetPuzzle" $ \p ->
      let input = encode $ object ["puzzle" .= p, "user" .= String "foo"]
          expected = Right $ withUser "foo" (SetPuzzle (puzzle p))
       in eitherDecode input `shouldBe` expected

    prop "SubmitSolution" $ \(User user) solution ->
      let input = encode $ object ["user" .= user, "solution" .= solution]
          expected = WithUser (User user) (SubmitSolution (Word solution))
       in eitherDecode input `shouldBe` Right expected

    prop "SubmitUnsolution" $ \text ->
      let input = encode $ object ["text" .= text]
          expected = SubmitUnsolution text
       in eitherDecode input `shouldBe` Right expected
