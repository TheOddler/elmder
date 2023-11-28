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
  { getMyInfo :: mode :- "me" :> Get '[JSON] User,
    findLoveForMe :: mode :- "me" :> "findLove" :> Get '[JSON] [UserID],
    getOthers :: mode :- "byIds" :> ReqBody '[JSON] [UserID] :> Post '[JSON] [User],
    getProfileSections :: mode :- Capture "userID" UserID :> "profileSections" :> Get '[JSON] [User.ProfileSection]
  }
  deriving (Generic)

pretendMyID :: UserID
pretendMyID = UserID 1

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getMyInfo = do
        users <- runHasql $ User.getUsers [pretendMyID]
        case users of
          [user] -> pure user
          _ -> throwError $ err500 {errBody = "Could not find myself"},
      findLoveForMe = do
        let wanted = 10
        ensureSomeUsersInDB wanted
        runHasql $ User.findPotentialLoveFor pretendMyID wanted,
      getOthers = runHasql . User.getUsers,
      getProfileSections = runHasql . User.getProfileSections
    }
