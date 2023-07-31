{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB where

import AppM (AppM, AppState (appStateConnectionPool))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Text (Text)
import Database.Persist.Postgresql
import Database.Persist.TH

runDb :: SqlPersistT IO a -> AppM a
runDb query = do
  pool <- asks appStateConnectionPool
  liftIO $ runSqlPool query pool

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
GreetedPerson
    name Text
    somethingElse Int
    deriving Show
|]

initDB :: ConnectionPool -> IO ()
initDB = runSqlPool (runMigration migrateAll)
