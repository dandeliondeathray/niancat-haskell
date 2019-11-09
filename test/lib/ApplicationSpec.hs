{-# LANGUAGE QuasiQuotes #-}

module ApplicationSpec where

import           Application
import           Service
import           Web

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Default.Class
import           Web.Scotty.Trans       (scottyAppT)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

app = do
  sync <- newTVarIO def :: IO (TVar NiancatState)
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyAppT runActionToIO niancat

spec =
  with app $ do
    describe "GET /" $ do
      it "returns a hello message" $ do
        get "/" `shouldRespondWith`
          [json|[{response_type:"reply",message:"Hello, niancat!"}]|]
      it "with status 200" $ do get "/" `shouldRespondWith` 200
