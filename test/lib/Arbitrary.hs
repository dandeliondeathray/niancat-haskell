module Arbitrary where

import           Application

import           Helpers

import           Control.Applicative
import           Data.Map                       (toList)
import           Prelude                        hiding (Word)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.Text

newtype Valid = Valid Word deriving (Show)
newtype Invalid = Invalid Word deriving (Show)

instance Arbitrary Word where
  arbitrary = word <$> arbitrary

instance Arbitrary Valid where
  arbitrary = do
    let (Dictionary d) = testDictionary
    let entries = snd =<< toList d
    w <- elements entries
    return . Valid $ w

instance Arbitrary Invalid where
  arbitrary = do
    w <- word <$> arbitrary
    if has testDictionary w
    then discard
    else return . Invalid $ w
