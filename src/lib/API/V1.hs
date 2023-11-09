{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module API.V1 where

import Data.Aeson
import Data.Text
import Features.Puzzle.Get
import Features.Puzzle.Set
import Features.Puzzle.Solve
import Features.Streaks
import Features.Unsolutions.Post
import Niancat.Dictionary
import Niancat.Domain
import Niancat.Puzzle
import Niancat.Replies
import Persistence.Events
import Servant
import Web

newtype V1 a = V1 a

type API =
  "puzzle" :> Get '[JSON] [Message]
    :<|> "puzzle" :> ReqBody '[JSON] (V1 (WithUser SetPuzzle)) :> Post '[JSON] [Message]
    :<|> "solution" :> ReqBody '[JSON] (WithUser SubmitSolution) :> Post '[JSON] [Message]
    :<|> "unsolution" :> Capture "user" Text :> ReqBody '[JSON] (V1 SubmitUnsolution) :> Post '[JSON] [Message]

api :: (Store s) => Dictionary -> ServerT API (AppM s)
api dict =
  query getPuzzle
    :<|> command (v1 (setPuzzle dict)) ><>> project streaks
    :<|> command (solvePuzzle dict)
    :<|> withUserFromPath (command $ v1u submitUnsolution)

v1 :: (a -> b) -> V1 a -> b
v1 f (V1 x) = f x

v1u :: (WithUser a -> b) -> WithUser (V1 a) -> b
v1u f (WithUser u (V1 x)) = f (WithUser u x)

instance FromJSON (V1 (WithUser SetPuzzle)) where
  parseJSON = withObject "puzzle" $ \o -> do
    p <- puzzle <$> o .: "puzzle"
    return $ V1 $ WithUser "" $ SetPuzzle p

instance FromJSON (V1 SubmitUnsolution) where
  parseJSON = withObject "unsolution" $ \o ->
    V1 . SubmitUnsolution <$> (o .: "unsolution")
