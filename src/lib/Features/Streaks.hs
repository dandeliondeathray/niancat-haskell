module Features.Streaks where

import Data.Default.Class
import Data.List hiding (intercalate, lines, lookup, map)
import Data.Map hiding (foldl)
import Data.Ord
import Data.Text hiding (elem, empty, foldl, lines, map, unpack)
import Data.Time
import Niancat.Domain hiding (apply)
import Niancat.Replies
import Persistence.Events
import Text.Printf (printf)
import Prelude hiding (lines, lookup, map)

data StreakState = SS
  { unbroken :: Map User Int,
    currentScore :: Map User Int,
    counts :: Bool
  }
  deriving (Show, Eq)

instance Default StreakState where
  def = SS {currentScore = empty, unbroken = empty, counts = False}

march :: StreakState -> EventWithMeta -> StreakState
march s (Imbued e (Meta u ts)) = applyEvent e
  where
    applyEvent (PuzzleSet _) | counts s = countScore
    applyEvent (PuzzleSet _) = s {counts = newPuzzleCounts}
    applyEvent (CorrectSolutionSubmitted _ (FirstTime True)) | counts s = s {currentScore = insertWith (+) u 1 $ currentScore s}
    applyEvent _ = s

    newPuzzleCounts = dayOfWeek (utctDay ts) `elem` [Monday, Tuesday, Wednesday, Thursday, Friday]

    countScore =
      SS
        { currentScore = empty,
          unbroken = filtered (currentScore s) $ tally (unbroken s) (currentScore s),
          counts = newPuzzleCounts
        }
      where
        tally :: Map User Int -> Map User Int -> Map User Int
        tally ub cs = foldl tallyOne ub $ toList cs
          where
            tallyOne :: Map User Int -> (User, Int) -> Map User Int
            tallyOne m (u', c) = insertWith (+) u' c m
        filtered cs = filterWithKey (\k _ -> k `member` cs)

streaks :: [EventWithMeta] -> StreaksReport
streaks = report . unbroken . foldl march def

newtype StreaksReport = Streaks (Map Int [User]) deriving (Show, Eq)

report :: Map User Int -> StreaksReport
report = Streaks . map sort . foldlWithKey (\m u c -> insertWith (++) c [u] m) empty

instance Response StreaksReport where
  messages (Streaks m) | m == empty = []
  messages (Streaks m) = [Notification $ intercalate "\n" (header : lines m)]
    where
      header :: Text
      header = "**Obrutna serier:**"
      lines :: Map Int [User] -> [Text]
      lines = fmap line . sortBy (comparing (Down . fst)) . toList
      line :: (Int, [User]) -> Text
      line (c, users) = pack $ printf "%d: %s" c (commaSeparated $ sort users)

      commaSeparated users = intercalate ", " $ fmap unpack users
        where
          unpack (User u) = u
