module Context where

import Control.Concurrent.STM
import Niancat.Domain
import Persistence.InMemory
import Persistence.Events

data (Store s) => Ctx s = Ctx
  { state :: TVar NiancatState
  , store :: s
  }

initialize :: NiancatState -> IO (Ctx InMemoryStore)
initialize initialState = do
  state' <- newTVarIO initialState
  store' <- newInMemoryStore

  return Ctx{state = state', store = store'}
