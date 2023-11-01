module Context where

import Control.Concurrent.STM
import Data.Time (UTCTime, getCurrentTime)
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
  let clock' = getCurrentTime
  return Ctx {state = state', store = store', clock = clock'}
