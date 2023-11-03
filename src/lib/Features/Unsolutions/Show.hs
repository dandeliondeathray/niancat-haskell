module Features.Unsolutions.Show where

import Data.Map hiding (foldl)
import Data.Text hiding (foldl)
import Niancat.Domain
import Niancat.Events
import Persistence.Events

newtype Unsolutions = Unsolutions (Map User [Text])

showUnsolutionsForUser :: User -> [EventWithMeta] -> Unsolutions
showUnsolutionsForUser u = Unsolutions . foldl (flip march) mempty
  where
    march (Imbued (UnsolutionSaved s) (Meta u' _)) | u == u' = insertWith (++) u [s]
    march _ = id
