module Niancat.PuzzleSpec where

import Data.List
import Data.Text
import GHC.Exts
import Niancat.Puzzle
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = describe "Puzzles are equal" $ do
  prop "if they have the same string" $ \x ->
    puzzle x == puzzle x `shouldBe` True
  prop "if they have the same letters, but in different order" $ \x ->
    let sorted = fromList . sort . toList
     in puzzle (sorted x) `shouldBe` puzzle x
  prop "if they have the same letters, but in different casing" $ \x ->
    puzzle (toLower x) `shouldBe` puzzle x
  it "if they differ only on a well-defined set of diacritics" $ do
    let denormalized = "éèÉÈáàÁÀåÅäÄöÖ"
        fixed = "eeEEaaAAåÅäÄöÖ"
     in puzzle denormalized `shouldBe` puzzle fixed
  prop "if they differ only by disallowed characters" $ \x ->
    puzzle ("_ -" `append` x) `shouldBe` puzzle x
