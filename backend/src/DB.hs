module DB where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Hasql.Connection qualified
import Hasql.Pool qualified
import Hasql.Session qualified
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
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
runHasql =
  -- ReadCommitted is the default of Postgres, so we make it the default for us too
  runHasqlWithIsolationLevel ReadCommitted

runHasqlWithIsolationLevel :: IsolationLevel -> Transaction result -> ServerM result
runHasqlWithIsolationLevel isolationLevel sql =
  -- Read/Write mode: Postgres doesn't actually do any optimisations for read-only
  -- it's only as a safety check, disallowing any updates. So it doesn't really matter
  -- to make all queries write queries, and that's one less thing to think about for now.
  runSession $ transaction isolationLevel Write sql
