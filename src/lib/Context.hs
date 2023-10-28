module Context where

import Control.Concurrent.STM
import Niancat.Domain
import Persistence.InMemory

data Ctx = Ctx
  { state :: TVar NiancatState
  , store :: InMemoryStore
  }