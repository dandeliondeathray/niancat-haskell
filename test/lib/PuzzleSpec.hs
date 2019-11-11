module PuzzleSpec where

import           Puzzle

import           Data.List
import           Data.Text.Lazy
import           GHC.Exts

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances.Text

spec :: Spec
spec = do
  describe "Puzzles are equal" $ do
    prop "if they have the same string" $ \x ->
      Puzzle x == Puzzle x `shouldBe` True
    prop "if they have the same letters, but in different order" $ \x ->
      let sorted = fromList . sort . toList
       in Puzzle (sorted x) `shouldBe` Puzzle x
    prop "if they have the same letters, but in different casing" $ \x ->
      Puzzle (toLower x) `shouldBe` Puzzle x
    prop "if they differ only on a well-defined set of diacritics" $ \x ->
      let denormalized = "éèÉÈáàÁÀ" `append` x
          fixed = "eeEEaaAA" `append` x
       in Puzzle denormalized `shouldBe` Puzzle fixed
    prop "if they differ only by disallowed characters" $ \x ->
      Puzzle ("_ -" `append` x) `shouldBe` Puzzle x
