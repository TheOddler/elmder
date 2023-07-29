{-# LANGUAGE OverloadedStrings #-}

module App where

import AppM (AppState (..), toServantHandler)
import DB (initConnectionPool, initDB)
import Network.Wai.Handler.Warp (run)
import Servant (Server, hoistServer, serve)
import Web (Api, apiProxy, routes)

mkServer :: AppState -> Server Api
mkServer appState = hoistServer apiProxy (toServantHandler appState) routes

main :: IO ()
main = do
  let connStr = "host=localhost"
  initDB connStr -- Until we have a proper migration system
  dbConns <- initConnectionPool connStr
  let appState = AppState dbConns
  run 8081 $ serve apiProxy $ mkServer appState
