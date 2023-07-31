{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Web where

import AppM (AppM)
import DB (GreetedPerson (..), runDb)
import Data.Text (Text)
import Database.Persist.Postgresql (Entity (..), insert, selectList)
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
  runDb $ do
    _ <- insert $ DB.GreetedPerson name 1
    greetedPeople <- selectList [] []
    pure $ (\gp -> "Hello " <> greetedPersonName (entityVal gp)) <$> greetedPeople
