{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Service where

import           Application
import           Features.GetPuzzle
import           Features.Hello
import           Features.SetPuzzle
import           Web

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.Default.Class
import           Data.String
import           Data.Text.Lazy                       (Text)

import           Network.Wai.Middleware.RequestLogger

import           Prelude

import           Web.Scotty.Trans

niancat :: Handler
niancat = do
  hello
  getPuzzle
  setPuzzle

runNiancat :: IO ()
runNiancat = do
  sync <- newTVarIO def :: IO (TVar NiancatState)
        -- 'runActionToIO' is called once per action.
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyT 3000 runActionToIO $ do
    middleware logStdoutDev
    niancat
