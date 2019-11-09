{-# LANGUAGE QuasiQuotes #-}

module ApplicationSpec where

import           Application

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Web.Scotty          (scottyApp)

app = scottyApp niancat

spec =
  with app $ do
    describe "GET /" $ do
      it "returns a hello message" $ do
        get "/" `shouldRespondWith` [json|"Hello, niancat!"|]
      it "with status 200" $ do get "/" `shouldRespondWith` 200
