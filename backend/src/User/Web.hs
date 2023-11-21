{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Web where

import Data.Int (Int32)
import GHC.Generics (Generic)
import Servant
  ( GenericMode ((:-)),
    Get,
    JSON,
    Post,
    ReqBody,
    (:>),
  )
import Servant.Server.Generic (AsServerT)
import ServerM (ServerM)
import User (User, UserID (UserID))
import User qualified

data UserRoutes mode = UserRoutes
  { getUsers :: mode :- "getMany" :> ReqBody '[JSON] [UserID] :> Post '[JSON] [User],
    getSomeUserIDs :: mode :- "exampleIDs" :> Get '[JSON] [UserID]
  }
  deriving (Generic)

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getUsers = User.getUsers,
      getSomeUserIDs = pure $ UserID <$> [1 .. 10 :: Int32]
    }
