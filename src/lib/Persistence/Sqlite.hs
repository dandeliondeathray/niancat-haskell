{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Persistence.Sqlite where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy hiding (intercalate)
import Data.Either
import Data.List
import Data.Text hiding (intercalate)
import Database.Selda hiding (Text)
import Database.Selda.SQLite (withSQLite)
import Niancat.Domain
import Persistence.Events hiding (eventData, eventType, timestamp, user)
import qualified Persistence.Events as PE

data EventRecord = Record
  { user :: Text,
    timestamp :: UTCTime,
    eventType :: Text,
    eventData :: ByteString
  }
  deriving (Eq, Show, Generic)

instance SqlRow EventRecord

instance ToEventWithMeta EventRecord where
  unmarshal record = do
    let t = eventType record
    p <- parserFor t
    d <- eitherDecode $ eventData record
    case parse p d of
      Error s -> Left s
      Success a -> Right $ Imbued a (Meta u ts)
    where
      u = User $ user record
      ts = timestamp record

instance FromEventWithMeta EventRecord where
  marshal (Imbued e (Meta (User u) t)) =
    Record
      { user = u,
        timestamp = t,
        eventType = PE.eventType e,
        eventData = encode $ PE.eventData e
      }

events :: Table EventRecord
events = table "events" []

newtype SqliteStore = Sqlite
  { connStr :: FilePath
  }

allEvents :: Query s (Row s EventRecord)
allEvents = do
  events' <- select events
  _ <- order (events' ! #timestamp) ascending
  return events'

eventsSince :: UTCTime -> Query s (Row s EventRecord)
eventsSince ts = do
  events' <- select events
  _ <- order (events' ! #timestamp) ascending
  _ <- restrict (events' ! #timestamp .>= literal ts)
  return events'

instance Store SqliteStore where
  getAll s = liftIO $ withSQLite (connStr s) $ do
    records <- query allEvents
    return $ readRecords records

  getSince ts s = liftIO $ withSQLite (connStr s) $ do
    records <- query $ eventsSince ts
    return $ readRecords records

  append es s = liftIO $ withSQLite (connStr s) $ do
    insert_ events $ fmap marshal es

initSqlite :: FilePath -> IO SqliteStore
initSqlite cs = withSQLite cs $ do
  tryCreateTable events
  return Sqlite {connStr = cs}

readRecords :: [EventRecord] -> [EventWithMeta]
readRecords records =
  let (errs, es) = partitionEithers $ fmap unmarshal records
   in case errs of
        [] -> es
        _ -> error (show $ intercalate "\n" errs)
