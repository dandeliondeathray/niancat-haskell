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

msgs :: PuzzleResponse -> AppM [Message]
msgs (CurrentPuzzle p) = return [Reply . showt $ p]
msgs NoPuzzle          = return [Reply "Nian är inte satt."]

getPuzzle :: AppM PuzzleResponse
getPuzzle = do
  s <- getState
  return $ case puzzle s of
    Just p  -> CurrentPuzzle p
    Nothing -> NoPuzzle


type GetPuzzleAPI = "v2" :> "puzzle" :> Get '[JSON] [Message]
getPuzzleAPI :: Proxy GetPuzzleAPI
getPuzzleAPI = Proxy
