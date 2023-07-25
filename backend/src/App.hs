{-# LANGUAGE OverloadedStrings #-}

module App (main) where

import DB (initConnectionPool)
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Web (apiProxy, routes)

main :: IO ()
main = do
  dbConns <- initConnectionPool "host=localhost"
  run 8081 (serve apiProxy $ routes dbConns)
