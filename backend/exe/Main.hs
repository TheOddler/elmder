module Main where

import Server qualified

main :: IO ()
main = do
  putStrLn "Starting server..."
  Server.main
