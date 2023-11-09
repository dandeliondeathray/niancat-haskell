{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Arbitrary.Instance where

import Control.Applicative
import Data.Map (toList)
import Data.Time
import Data.Time.DSL
import Features.Streaks
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle
import Test.Helpers
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Gen ()
import Test.QuickCheck.Instances.Text ()
import Prelude hiding (Word)

newtype Valid = Valid Word deriving (Show)

newtype Invalid = Invalid Word deriving (Show)

instance Arbitrary Word where
  arbitrary = Word <$> arbitrary

instance Arbitrary Valid where
  arbitrary = do
    let (Dictionary d) = testDictionary
    let entries = snd =<< toList d
    w <- elements entries
    return . Valid $ w

instance Arbitrary Invalid where
  arbitrary = do
    w <- Word <$> arbitrary
    if has testDictionary w
      then discard
      else return . Invalid $ w

instance Arbitrary User where
  arbitrary = User <$> arbitrary

instance Arbitrary Puzzle where
  arbitrary = puzzle <$> arbitrary

instance Arbitrary StreakState where
  arbitrary = do
    c <- arbitrary
    ub <- arbitrary
    score <- arbitrary
    return SS {currentScore = score, unbroken = ub, counts = c}

newtype Weekday = Weekday UTCTime deriving (Show)

instance Arbitrary Weekday where
  arbitrary = Weekday <$> elements [monday, tuesday, wednesday, thursday, friday]

newtype Weekend = Weekend UTCTime deriving (Show)

instance Arbitrary Weekend where
  arbitrary = Weekend <$> elements [saturday, sunday]

instance Arbitrary UTCTime where
  arbitrary = elements [monday, tuesday, wednesday, thursday, friday, saturday, sunday]

instance Arbitrary FirstTime where
  arbitrary = FirstTime <$> arbitrary
