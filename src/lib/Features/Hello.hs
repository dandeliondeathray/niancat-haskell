module Features.Hello where

import           Application
import           Web

import           Data.Text.Lazy
import           Web.Scotty.Trans

data Hello =
  Hello Text

instance Query Hello where
  resolve _ (Hello who) = [Reply (mconcat ["Hello, ", who, "!"])]

instance FromRequest Hello where
  parse = do
    return $ Hello "niancat"

hello :: ScottyT Text WebM ()
hello = do
  get "/" $ do
    q <- parse :: Parsed Hello
    s <- webM $ getState
    json $ resolve s q
