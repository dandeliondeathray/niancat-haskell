{-# LANGUAGE MultiParamTypeClasses #-}

module Features.Hello where

import           Application
import           Web

import           Data.Maybe
import           Data.Text.Lazy

data Hello =
  Hello Text

data Greeting =
  Greeting Text

instance Query Hello Greeting where
  resolve (Hello who) _ = Greeting who

instance Response Greeting where
  messages (Greeting who) = [Reply (mconcat ["Hello, ", who, "!"])]

instance FromRequest Hello where
  parse = do
    who <- param "who"
    return . Hello $ fromMaybe "niancat" who

hello :: Handler
hello = do
  query GET "/" (parse :: Parser Hello) (resolve :: Resolver Hello Greeting)
