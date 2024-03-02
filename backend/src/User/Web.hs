{-# LANGUAGE DataKinds #-}

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
import User (UserAllInfo (..), UserID (..), UserOverviewInfo)
import User.Fake (ensureSomeUsersInDB)
import User.Impressions (Impression)
import User.Queries qualified as User

data UserRoutes mode = UserRoutes
  { getSearch :: mode :- "search" :> Get '[JSON] [UserOverviewInfo],
    getUserInfo :: mode :- Capture "userID" UserID :> "info" :> Get '[JSON] UserAllInfo,
    getImpressions :: mode :- "impressions" :> Capture "impression" Impression :> Get '[JSON] [UserOverviewInfo],
    postSetImpression :: mode :- Capture "impression" Impression :> Capture "otherUserID" UserID :> Post '[JSON] (),
    getMatches :: mode :- "matches" :> Get '[JSON] [UserOverviewInfo],
    getAdmirers :: mode :- "admirers" :> Get '[JSON] [UserOverviewInfo]
  }
  deriving (Generic)

-- | The user ID of the currently logged in user, until we have actual login code
pretendMyID :: UserID
pretendMyID = UserID 1

userRoutes :: UserRoutes (AsServerT ServerM)
userRoutes =
  UserRoutes
    { getSearch = do
        _ <- ensureSomeUsersInDB 10
        runHasql $ User.searchFor pretendMyID 10,
      getUserInfo = \otherUserID ->
        runHasql $ do
          info <- User.getUserInfo pretendMyID otherUserID
          extInfo <- User.getUserExtendedInfo pretendMyID otherUserID
          pure $ UserAllInfo info extInfo,
      getImpressions = runHasql . User.getUsersWithImpressionBy pretendMyID,
      postSetImpression = \u -> runHasql . User.setImpressionBy pretendMyID u,
      getMatches = runHasql $ User.getMatchesFor pretendMyID,
      getAdmirers = runHasql $ User.getUsersThatLike pretendMyID
    }
