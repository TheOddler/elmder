{-# LANGUAGE OverloadedStrings #-}

module DB where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL, execute_)
import Database.PostgreSQL.Simple qualified as DB

type DBConnectionString = ByteString

initDB :: DBConnectionString -> IO ()
initDB connStr = bracket (connectPostgreSQL connStr) close $ \conn -> do
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS greeted_people (name text not null)"
  return ()

initConnectionPool :: DBConnectionString -> IO (Pool DB.Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe