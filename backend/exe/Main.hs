module Main where

import App qualified

main :: IO ()
main = do
  putStrLn "Starting app..."
  App.main
