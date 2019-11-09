module Features.Hello where

import           Data.Text
import           Web.Scotty

hello :: ScottyM ()
hello = get "/" $ do json ("Hello, niancat!" :: Text)
