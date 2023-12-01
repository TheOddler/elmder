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
    (:>),
  )
import Servant.Server.Generic (AsServerT)
import ServerM (ServerM)
import User (UserExtendedInfo, UserID (..), UserOverviewInfo)
import User qualified
import User.Fake (ensureSomeUsersInDB)

data UserRoutes mode = UserRoutes
  { getSearch :: mode :- "search" :> Get '[JSON] [UserOverviewInfo],
    getUserExtendedInfo :: mode :- Capture "userID" UserID :> "profile" :> Get '[JSON] UserExtendedInfo,
    getLikes :: mode :- "likes" :> Get '[JSON] [UserOverviewInfo],
    postLikeUser :: mode :- Capture "likedUserID" UserID :> "like" :> Post '[JSON] ()
  }
  deriving (Generic)

-- | The user ID of the currently logged in user, until we have actual login code
pretendMyID :: UserID
pretendMyID = UserID 1

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getSearch = do
        ensureSomeUsersInDB 10
        runHasql $ User.searchFor pretendMyID 10,
      getUserExtendedInfo = runHasql . User.getUserExtendedInfo,
      getLikes = runHasql $ User.getLikedUsersFor pretendMyID,
      postLikeUser = runHasql . User.likeUserBy pretendMyID
    }
