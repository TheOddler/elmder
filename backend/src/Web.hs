{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import AppM (AppM, withDbConn)
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as DB
import GHC.Generics (Generic)
import Servant
  ( Capture,
    GenericMode (type (:-)),
    Get,
    JSON,
    NamedRoutes,
    Proxy (..),
    type (:>),
  )
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

routes :: ApiRoutes (AsServerT AppM)
routes =
  ApiRoutes
    { ping = pure "pong",
      pong = pure "ping",
      iAm = greet,
      userRoutes = User.userRoutes
    }

say :: String -> AppM String
say = pure

greet :: Text -> AppM [Text]
greet name = do
  names <- withDbConn $ \conn -> do
    _ <- DB.execute conn "INSERT INTO greeted_people VALUES (?)" (DB.Only name)
    DB.query_ conn "SELECT name FROM greeted_people"

  pure ((\n -> "Hello " <> DB.fromOnly n) <$> names)
