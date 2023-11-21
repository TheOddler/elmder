{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Hasql.Connection qualified
import Hasql.Pool qualified
import Hasql.Session qualified
import Hasql.Statement qualified
import Hasql.TH (uncheckedSql)
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

runHasql :: params -> Hasql.Statement.Statement params result -> ServerM result
runHasql params statement = do
  runSession $ Hasql.Session.statement params statement

-- | This function should initialize the database in an idempotent way
-- I use this until I have a proper migration setup for the database
initDB :: Hasql.Pool.Pool -> IO ()
initDB pool =
  runSessionWith pool $
    Hasql.Session.sql
      [uncheckedSql|
        CREATE TABLE IF NOT EXISTS greeted_people (name text NOT NULL);
        CREATE TYPE relationship_status AS ENUM ('single', 'married', 'in_relationship');
        CREATE TABLE IF NOT EXISTS users (
          id SERIAL PRIMARY KEY,
          name text NOT NULL,
          header_image_url text NOT NULL,
          description text NOT NULL,
          relationship_status relationship_status NOT NULL
        );
      |]
