module DictionarySpec where

import Arbitrary
import Data.Map
import Helpers
import Test.Hspec
import Test.Hspec.QuickCheck

import Niancat.Dictionary
import Niancat.Puzzle

spec :: Spec
spec = do
  describe "building a dictionary" $ do
    let input = ["short", "muchtoolong", "ninechars"]
    it "takes only words with 9 letters" $ do
      let (Dictionary dict) = build input
      elems dict `shouldBe` [[Word "NINECHARS"]]
    it "keys by Puzzle.key" $ do
      let (Dictionary dict) = build input
      keys dict `shouldBe` [key "ninechars"]
  describe "checking if a word is in the dictionary" $ do
    context "with an empty dictionary" $ do
      let dict = Dictionary mempty
      prop "no words are included" $ \w -> has dict w `shouldBe` False
    context "with the test dictionary" $ do
      let dict = testDictionary
      prop "valid words are included" $ \(Valid w) -> has dict w `shouldBe` True
      prop "invalid words are not" $ \(Invalid w) -> has dict w `shouldBe` False
