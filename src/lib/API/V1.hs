{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module API.V1 where

import Data.Aeson
import Features.GetPuzzle
import Features.SetPuzzle
import Features.SolvePuzzle
import Features.Streaks
import Features.Unsolutions
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
    :<|> "unsolution" :> Capture "user" (V1 User) :> ReqBody '[JSON] (V1 SubmitUnsolution) :> Post '[JSON] [Message]

api :: (Store s) => Dictionary -> ServerT API (AppM s)
api dict =
  query getPuzzle
    :<|> (\r -> (++) <$> command (v1 (setPuzzle dict) r) <*> project streaks)
    :<|> command . solvePuzzle dict
    :<|> (\u -> command . v1u u submitUnsolution)

v1 :: (a -> b) -> V1 a -> b
v1 f (V1 x) = f x

v1u :: V1 User -> (WithUser a -> b) -> V1 a -> b
v1u (V1 u) f (V1 x) = f (WithUser u x)

instance FromJSON (V1 (WithUser SetPuzzle)) where
  parseJSON = withObject "puzzle" $ \o -> do
    p <- puzzle <$> o .: "puzzle"
    return $ V1 $ WithUser "" $ SetPuzzle p

instance FromJSON (V1 SubmitUnsolution) where
  parseJSON = withObject "unsolution" $ \o ->
    V1 . SubmitUnsolution <$> (o .: "unsolution")

instance FromHttpApiData (V1 User) where
  parseUrlPiece = Right . V1 . User
