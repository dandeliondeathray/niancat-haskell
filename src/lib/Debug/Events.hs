module Debug.Events where

import Context
import Persistence.Events

events :: Ctx -> IO [Serializable]
events = fmap (fmap Serializable) . getAll . store
