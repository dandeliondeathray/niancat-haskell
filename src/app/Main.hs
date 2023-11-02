module Main where

import Context
import Data.Default.Class
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Service

main :: IO ()
main = do
  dictionary <- getDictionary
  ctx <- initialize def
  let a = niancat dictionary ctx
  putStrLn "Serving niancat on port 3000"
  run 3000 . logStdoutDev $ a
