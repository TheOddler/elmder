{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic (AsServerT)
import User (UserRoutes)
import User qualified

type Api = NamedRoutes ApiRoutes

data ApiRoutes mode = ApiRoutes
  { ping :: mode :- "ping" :> Get '[JSON] String,
    pong :: mode :- "pong" :> Get '[JSON] String,
    iAm :: mode :- "iAm" :> Capture "name" String :> Get '[JSON] String,
    userRoutes :: mode :- "user" :> NamedRoutes UserRoutes
  }
  deriving (Generic)

apiProxy :: Proxy Api
apiProxy = Proxy

routes :: ApiRoutes (AsServerT Handler)
routes =
  ApiRoutes
    { ping = pure "pong",
      pong = pure "ping",
      iAm = greet,
      userRoutes = User.userRoutes
    }

say :: String -> Handler String
say = pure

greet :: String -> Handler String
greet name = pure $ "Hello " <> name
