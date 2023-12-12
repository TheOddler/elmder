module ServerM where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Hasql.Pool qualified
import Servant qualified

newtype ServerEnv = ServerEnv
  { dbConnectionPool :: Hasql.Pool.Pool
  }

type ServerM = ReaderT ServerEnv Servant.Handler

toServantHandler :: ServerEnv -> ServerM a -> Servant.Handler a
toServantHandler = flip runReaderT
