{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AppM where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Hasql.Pool qualified
import Servant qualified

newtype AppState = AppState
  { dbConnectionPool :: Hasql.Pool.Pool
  }

type AppM = ReaderT AppState Servant.Handler

toServantHandler :: AppState -> AppM a -> Servant.Handler a
toServantHandler = flip runReaderT
