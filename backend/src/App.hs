{-# LANGUAGE OverloadedStrings #-}

module App where

import AppM (AppState (..), toServantHandler)
import DB (initConnectionPool, initDB)
import Hasql.Connection (Settings)
import Network.Wai.Handler.Warp (run)
import Servant (Server, hoistServer, serve)
import Web (Api, apiProxy, routes)

mkServer :: Settings -> IO (Server Api)
mkServer connStr = do
  pool <- initConnectionPool connStr
  initDB pool -- Until we have a proper migration system
  let appState = AppState pool
  pure $ hoistServer apiProxy (toServantHandler appState) routes

main :: IO ()
main = do
  let connStr = "host=127.0.0.1"
  server <- mkServer connStr
  run 8081 $ serve apiProxy server
