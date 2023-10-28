module Context where

import Control.Concurrent.STM
import Niancat.Domain
import Persistence.Events
import Persistence.Sqlite

data (Store s) => Ctx s = Ctx
  { state :: TVar NiancatState
  , store :: s
  }

initialize :: NiancatState -> IO (Ctx SqliteStore)
initialize initialState = do
  state' <- newTVarIO initialState
  store' <- initSqlite "niancat.sqlite"

  return Ctx{state = state', store = store'}
