{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module API.V2 where

import Data.Text
import Features.Puzzle.Get
import Features.Puzzle.Set
import Features.Puzzle.Solve
import Features.Streaks
import Features.Unsolutions.Post
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Replies
import Persistence.Events
import Servant
import Web

type API =
  "puzzle" :> Get '[JSON] [Message]
    :<|> "puzzle" :> ReqBody '[JSON] (WithUser SetPuzzle) :> Put '[JSON] [Message]
    :<|> "solutions" :> ReqBody '[JSON] (WithUser SubmitSolution) :> Post '[JSON] [Message]
    :<|> "streaks" :> Get '[JSON] [Message]
    :<|> "unsolutions" :> Capture "user" Text :> ReqBody '[JSON] SubmitUnsolution :> Post '[JSON] [Message]

api :: (Store s) => Dictionary -> ServerT API (AppM s)
api dict =
  query getPuzzle
    :<|> (\req -> (++) <$> command (setPuzzle dict req) <*> project streaks)
    :<|> command . solvePuzzle dict
    :<|> project streaks
    :<|> (\u -> command . submitUnsolution (User u))
