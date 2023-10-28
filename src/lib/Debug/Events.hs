module Debug.Events where

import Context
import Persistence.Events

events :: (Store s) => Ctx s -> IO [EventWithMeta]
events = getAll . store
