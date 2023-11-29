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
    Post,
    ReqBody,
    ServerError (..),
    err500,
    throwError,
    (:>),
  )
import Servant.Server.Generic (AsServerT)
import ServerM (ServerM)
import User (User, UserID (..))
import User qualified
import User.Fake (ensureSomeUsersInDB)

data UserRoutes mode = UserRoutes
  { getMe :: mode :- "me" :> Get '[JSON] User,
    getSearch :: mode :- "search" :> Get '[JSON] [UserID],
    getByIds :: mode :- "byIds" :> ReqBody '[JSON] [UserID] :> Post '[JSON] [User],
    getProfileSections :: mode :- Capture "userID" UserID :> "profileSections" :> Get '[JSON] [User.ProfileSection]
  }
  deriving (Generic)

pretendMyID :: UserID
pretendMyID = UserID 1

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getMe = do
        users <- runHasql $ User.getUsers [pretendMyID]
        case users of
          [user] -> pure user
          _ -> throwError $ err500 {errBody = "Could not find myself"},
      getSearch = do
        let wanted = 10
        ensureSomeUsersInDB wanted
        runHasql $ User.searchFor pretendMyID wanted,
      getByIds = runHasql . User.getUsers,
      getProfileSections = runHasql . User.getProfileSections
    }
