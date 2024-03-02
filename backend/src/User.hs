{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (Day)
import DeriveElmAndJson (deriveElmAndJson)
import GHC.Generics (Generic)
import SafeMaths (roundToNearest)
import Servant (FromHttpApiData, ToHttpApiData)
import User.GenderIdentity (GenderIdentity)
import User.Impressions (Impression)
import Util (reverseEnumToText)

newtype UserID = UserID {unUserID :: Int32}
  deriving (Generic, Show, Eq)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | This is the info about a user that we show on the overview.
-- Any additional information is only visible when looking at the full profile
-- See 'UserExtendedInfo' for more information about a user.
data UserOverviewInfo = UserOverviewInfo
  { userId :: UserID,
    userName :: Text,
    userHeaderImageUrl :: Text,
    -- | For privacy we never tell someone the exact location of someone else, even though we do store it in the database.
    -- We'll round this based on the user's settings.
    userDistanceM :: Int,
    -- | For privacy we send the age as a number to the frontend. Of course we do store the birthday as a data in the database.
    userAge :: Int,
    userGenderIdentity :: GenderIdentity,
    -- | The impression that the currently logged in user has of this user, if any
    userImpression :: Maybe Impression
  }
  deriving (Generic, Show, Eq)

-- | Additional information about a user
-- This should not include information already in 'UserOverviewInfo' as the
-- frontend might already know this when it requests this information.
data UserExtendedInfo = UserExtendedInfo
  { userExtProfileAge :: Int,
    userExtDescription :: Text,
    userExtProfileSections :: [ProfileSection],
    userExtRelationshipStatus :: RelationshipStatus, -- TODO: Rework this to a web of relationships

    -- | The impression that the currently logged in user has of this user, if any
    userExtImpression :: Maybe Impression
  }
  deriving (Generic, Show, Eq)

data UserAllInfo = UserAllInfo
  { userAllOverviewInfo :: UserOverviewInfo,
    userAllExtendedInfo :: UserExtendedInfo
  }
  deriving (Generic, Show, Eq)

data RelationshipStatus
  = RelationshipStatusUnknown
  | RelationshipStatusSingle
  | RelationshipStatusMarried
  | RelationshipStatusInRelationship
  deriving (Generic, Show, Eq, Enum, Bounded)

-- | Convert a 'RelationshipStatus' to a SQL name for it.
relationshipStatusToSQL :: RelationshipStatus -> Text
relationshipStatusToSQL = \case
  RelationshipStatusUnknown -> "unknown"
  RelationshipStatusSingle -> "single"
  RelationshipStatusInRelationship -> "in_relationship"
  RelationshipStatusMarried -> "married"

sqlToRelationshipStatus :: Text -> Maybe RelationshipStatus
sqlToRelationshipStatus = reverseEnumToText relationshipStatusToSQL

data ProfileSection
  = UserSectionGeneric
      { userSectionGenericHeader :: Text,
        userSectionGenericContent :: Text
      }
  | UserSectionImages
      { userSectionImagesImages :: [Text],
        userSectionImagesDescription :: Text
      }
  | UserSectionQuestionAndAnswer
      { userSectionQuestionAndAnswerQuestion :: Text,
        userSectionQuestionAndAnswerAnswer :: Text
      }
  deriving (Generic, Show, Eq)

-- Use the default options from Servant.Elm, they work best for Elm, not those from Elm.Derive nor Aeson.
deriveElmAndJson ''UserID
deriveElmAndJson ''RelationshipStatus
deriveElmAndJson ''ProfileSection
deriveElmAndJson ''UserOverviewInfo
deriveElmAndJson ''UserExtendedInfo
deriveElmAndJson ''UserAllInfo

data NewUserInfo = NewUserInfo
  { newUserName :: Text,
    newUserDescription :: Text,
    newUserHeaderImageUrl :: Text,
    newUserLatitude :: Float,
    newUserLongitude :: Float,
    newUserBirthday :: Day,
    newUserGenderIdentity :: GenderIdentity,
    newUserSearchMinAge :: Int32,
    newUserSearchMaxAge :: Int32,
    newUserSearchDistanceM :: Int32,
    newUserSearchGenderIdentities :: [GenderIdentity]
  }

smartRoundDistanceM :: Float -> Float -> Int
smartRoundDistanceM distM searchDistM =
  let distanceM :: Int
      distanceM = round distM
      searchDistanceM :: Int
      searchDistanceM = round searchDistM
   in case distanceM of
        m | m < 150 && searchDistanceM <= 1000 -> 100
        m | m <= 1000 && searchDistanceM <= 1000 -> roundToNearest 100 m
        m | m <= 1000 -> 1000
        m | m <= 3000 -> roundToNearest 500 m
        m -> roundToNearest 1000 m
