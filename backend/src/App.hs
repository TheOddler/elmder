{-# LANGUAGE OverloadedStrings #-}

module App where

import AppM (AppState (..), toServantHandler)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import DB (initDB)
import Database.Persist.Postgresql (withPostgresqlPool)
import Network.Wai.Handler.Warp (run)
import Servant (Server, hoistServer, serve)
import Web (Api, apiProxy, routes)

mkServer :: AppState -> Server Api
mkServer appState = hoistServer apiProxy (toServantHandler appState) routes

main :: IO ()
main = do
  let connStr = "host=localhost"
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \dbConns ->
    liftIO $ do
      let appState = AppState dbConns
      initDB dbConns
      run 8081 $ serve apiProxy $ mkServer appState
