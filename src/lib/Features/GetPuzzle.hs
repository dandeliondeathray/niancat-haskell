{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Features.GetPuzzle where

import           Application
import           Web

import           Servant
import           TextShow

data GetPuzzle =
  GetPuzzle deriving(Show,Eq)

data PuzzleResponse
  = CurrentPuzzle Puzzle
  | NoPuzzle

instance Response PuzzleResponse where
  messages (CurrentPuzzle p) = [Reply . showt $ p]
  messages NoPuzzle          = [Reply "Nian är inte satt."]

getPuzzle :: AppM [Message]
getPuzzle = query (maybe NoPuzzle CurrentPuzzle . puzzle)

type GetPuzzleAPI = "v2" :> "puzzle" :> Get '[JSON] [Message]
getPuzzleAPI :: Proxy GetPuzzleAPI
getPuzzleAPI = Proxy
