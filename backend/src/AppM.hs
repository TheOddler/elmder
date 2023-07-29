{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AppM where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple qualified as DB
import Servant qualified

newtype AppState = AppState
  { dbConnectionPool :: Pool DB.Connection
  }

type AppM = ReaderT AppState Servant.Handler

toServantHandler :: AppState -> AppM a -> Servant.Handler a
toServantHandler = flip runReaderT

withDbConn :: (DB.Connection -> IO a) -> AppM a
withDbConn f = do
  conns <- asks dbConnectionPool
  liftIO $ withResource conns f
