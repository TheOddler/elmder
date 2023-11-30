{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Web where

import DB (runHasql)
import GHC.Generics (Generic)
import Servant
  ( Capture,
    GenericMode ((:-)),
    Get,
    JSON,
    (:>),
  )
import Servant.Server.Generic (AsServerT)
import ServerM (ServerM)
import User (UserExtendedInfo, UserID (..), UserOverviewInfo)
import User qualified
import User.Fake (ensureSomeUsersInDB)

data UserRoutes mode = UserRoutes
  { getSearch :: mode :- "search" :> Get '[JSON] [UserOverviewInfo],
    getUserExtendedInfo :: mode :- Capture "userID" UserID :> "profile" :> Get '[JSON] UserExtendedInfo
  }
  deriving (Generic)

pretendMyID :: UserID
pretendMyID = UserID 1

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getSearch = do
        ensureSomeUsersInDB 10
        runHasql $ User.searchFor pretendMyID 10,
      getUserExtendedInfo = runHasql . User.getUserExtendedInfo
    }
