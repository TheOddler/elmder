{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as DB
import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic (AsServerT)
import User (UserRoutes)
import User qualified

type Api = NamedRoutes ApiRoutes

data ApiRoutes mode = ApiRoutes
  { ping :: mode :- "ping" :> Get '[JSON] String,
    pong :: mode :- "pong" :> Get '[JSON] String,
    iAm :: mode :- "iAm" :> Capture "name" Text :> Get '[JSON] [Text],
    userRoutes :: mode :- "user" :> NamedRoutes UserRoutes
  }
  deriving (Generic)

apiProxy :: Proxy Api
apiProxy = Proxy

routes :: Pool DB.Connection -> ApiRoutes (AsServerT Handler)
routes dbConns =
  ApiRoutes
    { ping = pure "pong",
      pong = pure "ping",
      iAm = greet dbConns,
      userRoutes = User.userRoutes dbConns
    }

say :: String -> Handler String
say = pure

greet :: Pool DB.Connection -> Text -> Handler [Text]
greet dbConns name = do
  names <- liftIO $ withResource dbConns $ \conn -> do
    _ <- DB.execute conn "INSERT INTO greeted_people VALUES (?)" (DB.Only name)
    DB.query_ conn "SELECT name FROM greeted_people"

  pure ((\n -> "Hello " <> DB.fromOnly n) <$> names)
