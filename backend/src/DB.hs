{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module DB where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Hasql.Connection qualified
import Hasql.Pool qualified
import Hasql.Session qualified
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (..), transaction)
import ServerM (ServerEnv (dbConnectionPool), ServerM)

initConnectionPool :: Hasql.Connection.Settings -> IO Hasql.Pool.Pool
initConnectionPool =
  Hasql.Pool.acquire
    10 -- Pool size
    (Just $ 10 * 1_000_000) -- Connection acquisition timeout in microseconds

runSessionWith :: Hasql.Pool.Pool -> Hasql.Session.Session a -> IO a
runSessionWith pool session = do
  errOrResult <- Hasql.Pool.use pool session
  case errOrResult of
    Left err -> throwIO err
    Right a -> pure a

runSession :: Hasql.Session.Session a -> ServerM a
runSession session = do
  pool <- asks dbConnectionPool
  liftIO $ runSessionWith pool session

runHasql :: Transaction result -> ServerM result
runHasql sql = do
  runSession $ transaction Serializable Write sql

readHasql :: Transaction result -> ServerM result
readHasql sql = do
  runSession $ transaction Serializable Read sql
