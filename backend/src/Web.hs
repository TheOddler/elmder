{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import DB (runHasql)
import Data.Foldable (toList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.TH (resultlessStatement, vectorStatement)
import Hasql.Transaction (statement)
import Search (SearchRoutes)
import Search qualified
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
import ServerM (ServerM)
import User.Web (UserRoutes)
import User.Web qualified as User

type Api = NamedRoutes ApiRoutes

data ApiRoutes mode = ApiRoutes
  { ping :: mode :- "ping" :> Get '[JSON] String,
    pong :: mode :- "pong" :> Get '[JSON] String,
    iAm :: mode :- "iAm" :> Capture "name" Text :> Get '[JSON] [Text],
    userRoutes :: mode :- "user" :> NamedRoutes UserRoutes,
    searchRoutes :: mode :- "search" :> NamedRoutes SearchRoutes
  }
  deriving (Generic)

apiProxy :: Proxy Api
apiProxy = Proxy

routes :: ApiRoutes (AsServerT ServerM)
routes =
  ApiRoutes
    { ping = pure "pong",
      pong = pure "ping",
      iAm = greet,
      userRoutes = User.userRoutes,
      searchRoutes = Search.searchRoutes
    }

say :: String -> ServerM String
say = pure

greet :: Text -> ServerM [Text]
greet name = do
  names <- runHasql $ do
    statement name [resultlessStatement| INSERT INTO greeted_people (name) VALUES ($1 :: text) |]
    toList <$> statement () [vectorStatement| SELECT name :: text FROM greeted_people |]

  pure (("Hello " <>) <$> names)
