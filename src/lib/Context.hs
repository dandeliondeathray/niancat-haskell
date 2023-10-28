module Context where

import Control.Concurrent.STM
import Niancat.Domain
import Persistence.InMemory

data Ctx = Ctx
  { state :: TVar NiancatState
  , store :: InMemoryStore
  }

initialize :: NiancatState -> IO Ctx
initialize initialState = do
  state' <- newTVarIO initialState
  events' <- newTVarIO []

  return Ctx { state = state', store = InMemory { events = events' } }
