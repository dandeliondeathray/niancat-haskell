module Debug.Events where

import Context
import Persistence.Events

events :: Ctx -> IO [StoredEvent]
events = getAll . store
