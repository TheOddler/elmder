{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import AppM (AppM)
import DB (runHasql)
import Data.Foldable (toList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.TH (resultlessStatement, vectorStatement)
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
  runHasql name [resultlessStatement| INSERT INTO greeted_people (name) VALUES ($1 :: text) |]
  names <- toList <$> runHasql () [vectorStatement| SELECT name :: text FROM greeted_people |]

  pure (("Hello " <>) <$> names)
