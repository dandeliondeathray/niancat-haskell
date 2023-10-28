module Debug.Events where

import Context
import Persistence.Events
import Persistence.InMemory

events :: Ctx -> IO [InMemoryEvent]
events = getAll . store
