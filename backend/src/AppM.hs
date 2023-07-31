{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AppM where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Database.Persist.Postgresql (ConnectionPool)
import Servant qualified

newtype AppState = AppState
  { appStateConnectionPool :: ConnectionPool
  }

type AppM = ReaderT AppState Servant.Handler

toServantHandler :: AppState -> AppM a -> Servant.Handler a
toServantHandler = flip runReaderT
