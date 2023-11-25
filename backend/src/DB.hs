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
import Hasql.Transaction.Sessions (IsolationLevel (RepeatableRead), Mode (..), transaction)
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
  -- Isolation level: RepeatableRead seems like a good default isolation level.
  -- I'm basing this on not that much knowledge though, so I might be wrong.
  -- It's supposedly the fastest one, and I don't think we need serializable anywhere.
  -- If we do, I'll add it at that point.

  -- Read/Write mode: Postgres doesn't actually do any optimisations for read-only
  -- it's only as a safety check, disallowing any updates. But for now I'm not
  -- going to bother with read-only transactions.

  runSession $ transaction RepeatableRead Write sql
