{-# LANGUAGE ScopedTypeVariables #-}

module SerializationSpec where

import Data.Aeson
import Data.Text.Lazy
import Features.SetPuzzle
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = do
  describe "Serialization of responses" $ do
    prop "Notification" $ \message ->
      let input = Notification message
          expected =
            encode $
              object
                [ "response_type" .= ("notification" :: Text),
                  "message" .= message
                ]
       in encode input `shouldBe` expected
    prop "Reply" $ \message ->
      let input = Reply message
          expected =
            encode $
              object ["response_type" .= ("reply" :: Text), "message" .= message]
       in encode input `shouldBe` expected
  describe "Deserialization of commands" $
    prop "SetPuzzle" $
      \p ->
        let input = encode $ object ["puzzle" .= p, "user" .= String "foo"]
            expected = Right $ withUser "foo" $ SetPuzzle (puzzle p)
            actual = eitherDecode input
         in actual `shouldBe` expected

--   prop "SubmitSolution" $ \(user :: Text, solution :: Text) ->
--     let input = encode $ object ["user" .= user, "solution" .= solution]
--         expected = SubmitSolution (User user) solution
--      in eitherDecode input `shouldBe` Right expected
--   prop "SubmitUnsolution" $ \(user :: Text, text :: Text) ->
--     let input = encode $ object ["text" .= text]
--         expected = SubmitUnsolution (User user) text
--         parsed = eitherDecode input
--         result = fmap (flip ($) user) parsed
--      in result `shouldBe` Right expected
