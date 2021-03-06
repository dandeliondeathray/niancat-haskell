{-# LANGUAGE ScopedTypeVariables #-}

module SerializationSpec where

import Data.Aeson
import Data.Text.Lazy
import Data.Typeable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text

import Features.SetPuzzle

import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies

import Web

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
    prop "SetPuzzle" $ \p ->
      let input = encode $ object ["puzzle" .= p]
          expected = Right $ SetPuzzle (puzzle p)
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
