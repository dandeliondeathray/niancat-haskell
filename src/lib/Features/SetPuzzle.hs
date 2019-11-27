{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Features.SetPuzzle where

import           Application
import           Web

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Aeson
import           Servant
import           TextShow

newtype SetPuzzle = SetPuzzle Puzzle deriving (Show, Eq)

data SetPuzzleResponse
  = PuzzleSet Puzzle
  | SamePuzzle Puzzle
  deriving (Show, Eq)

-- instance FromRequest SetPuzzle where
--   parse = jsonData

instance FromJSON SetPuzzle where
  parseJSON = withObject "puzzle" $ \o -> SetPuzzle . Puzzle <$> o .: "puzzle"

ms :: SetPuzzleResponse -> [Message]
ms (PuzzleSet p) =
  [Reply "OK!", Notification $ mconcat ["Dagens nia är **", showt p, "**"]]
ms (SamePuzzle p) =
  [Reply $ mconcat ["Nian är redan satt till ", showt p]]

app :: SetPuzzle -> NiancatState -> (NiancatState, SetPuzzleResponse)
app (SetPuzzle p') s =
  case puzzle s of
    Just p
      | p /= p' -> (s', PuzzleSet p')
    Nothing -> (s', PuzzleSet p')
    Just p
      | p == p' -> (s, SamePuzzle p)
  where
    s' = s {puzzle = Just p'}
    r = PuzzleSet p'

setPuzzle :: SetPuzzle -> AppM [Message]
setPuzzle cmd = do
  ts <- ask
  liftIO . atomically $ do
    s <- readTVar ts
    let (s', r) = app cmd s
    writeTVar ts s'
    return $ ms r

type SetPuzzleAPI = "v2" :> "puzzle" :> ReqBody '[JSON] SetPuzzle :> Put '[JSON] [Message]
setPuzzleAPI :: Proxy SetPuzzleAPI
setPuzzleAPI = Proxy

-- setPuzzle :: Handler
-- setPuzzle = do
--   command
--     PUT
--     "/v2/puzzle"
--     (parse :: Parser SetPuzzle)
--     (apply :: Applyer SetPuzzle SetPuzzleResponse)
