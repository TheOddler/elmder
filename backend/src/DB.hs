module DB where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Time (secondsToDiffTime)
import Hasql.Connection qualified
import Hasql.Interpolate (DecodeResult, Sql, interp)
import Hasql.Pool qualified
import Hasql.Session qualified
import Hasql.Transaction (Transaction, statement)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import ServerM (ServerEnv (dbConnectionPool), ServerM)

initConnectionPool :: Hasql.Connection.Settings -> IO Hasql.Pool.Pool
initConnectionPool =
  Hasql.Pool.acquire
    10 -- Pool size
    (secondsToDiffTime 10) -- Connection acquisition timeout.
    (secondsToDiffTime 10) -- Maximal connection lifetime.

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
runHasql =
  -- Isolation level: ReadCommitted is the default of Postgres,
  -- so we make it the default for us too
  -- Read/Write mode: Postgres doesn't actually do any optimisations for read-only
  -- it's only as a safety check, disallowing any updates. So it doesn't really matter
  -- to make all queries write queries, and that's one less thing to think about for now.
  runHasql' ReadCommitted Write

runHasql' :: IsolationLevel -> Mode -> Transaction result -> ServerM result
runHasql' isolationLevel readWriteMode sql =
  runSession $ transaction isolationLevel readWriteMode sql

sqlTransaction :: (DecodeResult a) => Sql -> Transaction a
sqlTransaction = statement () . interp False -- Should I prepare here instead? (False -> True)

sqlTransaction_ :: Sql -> Transaction ()
sqlTransaction_ = sqlTransaction
