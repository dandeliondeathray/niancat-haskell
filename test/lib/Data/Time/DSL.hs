module Data.Time.DSL where

import Data.Time

monday :: UTCTime
monday =
  UTCTime
    { utctDay = fromGregorian 2023 10 30,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

tuesday :: UTCTime
tuesday =
  UTCTime
    { utctDay = fromGregorian 2023 10 31,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

wednesday :: UTCTime
wednesday =
  UTCTime
    { utctDay = fromGregorian 2023 11 01,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

thursday :: UTCTime
thursday =
  UTCTime
    { utctDay = fromGregorian 2023 11 02,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

friday :: UTCTime
friday =
  UTCTime
    { utctDay = fromGregorian 2023 11 03,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

saturday :: UTCTime
saturday =
  UTCTime
    { utctDay = fromGregorian 2023 11 04,
      utctDayTime = secondsToDiffTime 32940
    }

sunday :: UTCTime
sunday =
  UTCTime
    { utctDay = fromGregorian 2023 11 05,
      utctDayTime = secondsToDiffTime 32940 -- 09:09
    }

t0 :: UTCTime
t0 = monday

dt :: NominalDiffTime
dt = secondsToNominalDiffTime 10

adv :: Int -> UTCTime -> UTCTime
adv 0 t = t
adv n t = adv (n - 1) (addUTCTime dt t)
