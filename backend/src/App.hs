{-# LANGUAGE OverloadedStrings #-}

module App where

import AppM (AppState (..), toServantHandler)
import DB (initConnectionPool, initDB)
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp (run)
import Servant (Server, hoistServer, serve)
import Web (Api, apiProxy, routes)

mkServer :: ByteString -> IO (Server Api)
mkServer connStr = do
  pool <- initConnectionPool connStr
  initDB pool -- Until we have a proper migration system
  let appState = AppState pool
  pure $ hoistServer apiProxy (toServantHandler appState) routes

main :: IO ()
main = do
  let connStr = "host=localhost"
  server <- mkServer connStr
  run 8081 $ serve apiProxy server
