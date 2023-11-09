module Features.Unsolutions.Show where

import Data.Bifunctor
import Data.List hiding (intercalate, lookup)
import Data.Map hiding (foldl)
import Data.Maybe
import Data.Ord
import Data.Text hiding (foldl, null)
import Data.Time
import Niancat.Domain
import Niancat.Events
import Niancat.Replies
import Persistence.Events
import Prelude hiding (lookup, null)

data UnsolutionsFor = UnsolutionsFor User [(UTCTime, Text)]

showUnsolutionsForUser :: User -> [EventWithMeta] -> UnsolutionsFor
showUnsolutionsForUser u = UnsolutionsFor u . fromMaybe [] . lookup u . foldl (flip march) mempty

instance Response UnsolutionsFor where
  messages (UnsolutionsFor _ []) = [Reply "Inga olösningar satta."]
  messages (UnsolutionsFor _ unsolutions) = [Reply $ listUnsolutions unsolutions]

newtype Unsolutions = Unsolutions (Map User [(UTCTime, Text)])

showUnsolutionsForEverybody :: [EventWithMeta] -> Unsolutions
showUnsolutionsForEverybody = Unsolutions . foldl (flip march) mempty

instance Response Unsolutions where
  messages (Unsolutions unsolutions) =
    fmap
      ( Reply
          . (\(u, us) -> u <> ":\n" <> us)
          . bimap (\(User u) -> u) listUnsolutions
      )
      . sortBy (comparing fst)
      . toList
      $ unsolutions

march :: EventWithMeta -> Map User [(UTCTime, Text)] -> Map User [(UTCTime, Text)]
march (Imbued (UnsolutionSaved s) (Meta u t)) = insertWith (++) u [(t, s)]
march (Imbued (PuzzleSet _) _) = const mempty
march _ = id

listUnsolutions :: [(UTCTime, Text)] -> Text
listUnsolutions = intercalate "\n" . fmap (("* " <>) . snd) . sortBy (comparing fst)
