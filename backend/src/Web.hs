{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import GHC.Generics
import Servant
import Servant.Server.Generic (AsServerT)

type Api = NamedRoutes ApiEndpoints

data ApiEndpoints mode = ApiEndpoints
  { ping :: mode :- "ping" :> Get '[JSON] String,
    pong :: mode :- "pong" :> Get '[JSON] String,
    iAm :: mode :- "iAm" :> Capture "name" String :> Get '[JSON] String
  }
  deriving (Generic)

apiProxy :: Proxy Api
apiProxy = Proxy

endpoints :: ApiEndpoints (AsServerT Handler)
endpoints =
  ApiEndpoints
    { ping = pure "pong",
      pong = pure "ping",
      iAm = greet
    }

say :: String -> Handler String
say = pure

greet :: String -> Handler String
greet name = pure $ "Hello " <> name
