{-# LANGUAGE OverloadedStrings #-}

module Server where

import DB (initConnectionPool)
import DB.Init (initDB)
import Hasql.Connection (Settings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (Server, hoistServer, serve)
import ServerM (ServerEnv (..), toServantHandler)
import Web (Api, apiProxy, routes)

mkServerEnv :: Settings -> IO ServerEnv
mkServerEnv dbConnStr = do
  pool <- initConnectionPool dbConnStr
  initDB pool -- Until we have a proper migration system
  pure $ ServerEnv pool

mkServer :: ServerEnv -> IO (Server Api)
mkServer serverEnv = do
  pure $ hoistServer apiProxy (toServantHandler serverEnv) routes

main :: IO ()
main = do
  let dbConnStr = "host=127.0.0.1"
  serverEnv <- mkServerEnv dbConnStr
  server <- mkServer serverEnv
  run 8081 $ simpleCors $ serve apiProxy server
