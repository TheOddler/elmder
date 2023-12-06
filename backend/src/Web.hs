{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import GHC.Generics (Generic)
import Servant
  ( GenericMode (type (:-)),
    Get,
    JSON,
    NamedRoutes,
    Proxy (..),
    type (:>),
  )
import Servant.Server.Generic (AsServerT)
import ServerM (ServerM)
import User.Web qualified as User

type Api = NamedRoutes ApiRoutes

data ApiRoutes mode = ApiRoutes
  { ping :: mode :- "ping" :> Get '[JSON] String,
    pong :: mode :- "pong" :> Get '[JSON] String,
    userRoutes :: mode :- "user" :> NamedRoutes User.UserRoutes
  }
  deriving (Generic)

apiProxy :: Proxy Api
apiProxy = Proxy

routes :: ApiRoutes (AsServerT ServerM)
routes =
  ApiRoutes
    { ping = pure "pong",
      pong = pure "ping",
      userRoutes = User.userRoutes
    }
