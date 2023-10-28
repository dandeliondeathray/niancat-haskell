module Debug.Events where

import Context
import Persistence.Events

events :: Ctx -> IO [EventWithMeta]
events = getAll . store
