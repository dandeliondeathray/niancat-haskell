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
import Features.Unsolutions.Show
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
    :<|> "unsolutions" :> Get '[JSON] [Message]
    :<|> "unsolutions" :> Capture "user" Text :> Get '[JSON] [Message]

api :: (Store s) => Dictionary -> ServerT API (AppM s)
api dict =
  query getPuzzle
    :<|> command (setPuzzle dict) ><>> project showUnsolutionsForEverybody ><>> project streaks
    :<|> command (solvePuzzle dict)
    :<|> project streaks
    :<|> withUserFromPath (command submitUnsolution)
    :<|> project showUnsolutionsForEverybody
    :<|> userFromPath (project1 showUnsolutionsForUser)
