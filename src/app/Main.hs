module Main where

import           Application

import           Web.Scotty

main :: IO ()
main = scotty 3000 $ niancat
