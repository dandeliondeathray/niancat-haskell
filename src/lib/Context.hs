module Context where

import Control.Concurrent.STM
import Data.Time
import Niancat.State
import Persistence.Events
import Persistence.Sqlite

data (Store s) => Ctx s = Ctx
  { state :: TVar NiancatState,
    store :: s,
    clock :: IO UTCTime
  }

initialize :: NiancatState -> IO (Ctx SqliteStore)
initialize initialState = do
  state' <- newTVarIO initialState
  store' <- initSqlite "niancat.sqlite"
  return Ctx {state = state', store = store', clock = getCurrentTime}
