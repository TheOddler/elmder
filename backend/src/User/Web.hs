{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Web where

import DB (runHasql)
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
import User (User, UserID)
import User qualified
import User.Fake (ensureSomeUsersInDB)

data UserRoutes mode = UserRoutes
  { getUsers :: mode :- "getMany" :> ReqBody '[JSON] [UserID] :> Post '[JSON] [User],
    getSomeUserIDs :: mode :- "exampleIDs" :> Get '[JSON] [UserID]
  }
  deriving (Generic)

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getUsers = runHasql . User.getUsers,
      getSomeUserIDs = do
        let wanted = 10
        ensureSomeUsersInDB wanted
        runHasql $ User.getSomeUserIDs wanted
    }
