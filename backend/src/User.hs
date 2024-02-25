{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import DB (sqlTransaction, sqlTransaction_)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day)
import DeriveElmAndJson (deriveElmAndJson)
import GHC.Generics (Generic)
import Hasql.Interpolate (EncodeValue, Sql, getOneColumn, getOneRow, sql)
import Hasql.Transaction (Transaction)
import SafeMaths (age, roundToNearest)
import Servant (FromHttpApiData, ToHttpApiData)
import User.GenderIdentity (GenderIdentity (..))
import User.Impressions (Impression (..))
import Util (reverseEnumToText)

newtype UserID = UserID {unUserID :: Int32}
  deriving (Generic, Show, Eq)
  deriving newtype (FromHttpApiData, ToHttpApiData, EncodeValue)

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
    userGenderIdentity :: GenderIdentity
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

getUserInfo :: UserID -> UserID -> Transaction UserOverviewInfo
getUserInfo myID otherID = do
  row <-
    sqlTransaction
      [sql|
        SELECT 
          other.id,
          other.name,
          other.header_image_url,
          earth_distance(me.location, other.location),
          CURRENT_DATE,
          other.birthday,
          other.gender_identity,
          (other.search_distance_km * 1000)
        FROM users me
        JOIN users other ON other.id = #{otherID}
        WHERE me.id = #{myID}
      |]
  pure $ userOverviewInfoFromSQL $ getOneRow row

getUserExtendedInfo :: UserID -> UserID -> Transaction UserExtendedInfo
getUserExtendedInfo myID otherID = do
  row <-
    sqlTransaction
      [sql|
        SELECT 
          join_day,
          CURRENT_DATE,
          description,
          relationship_status,
          impression
        FROM users
        LEFT JOIN impressions ON impressions.user_id = #{myID} AND impressions.other_user_id = #{otherID}
        WHERE id = #{otherID}
      |]
  pure $ fromSQL $ getOneRow row
  where
    fromSQL :: (Day, Day, Text, Text, Maybe Impression) -> UserExtendedInfo
    fromSQL (profileDate, currentDate, description, relationshipStatus, impression) =
      UserExtendedInfo
        { -- TODO: Implement actually getting this stuff from the DB
          userExtProfileAge = age currentDate profileDate,
          userExtDescription = description,
          userExtProfileSections =
            [ UserSectionGeneric "Generic header" "Generic content",
              UserSectionImages
                [ "https://via.placeholder.com/110",
                  "https://via.placeholder.com/120",
                  "https://via.placeholder.com/130",
                  "https://via.placeholder.com/140",
                  "https://via.placeholder.com/150"
                ]
                "Image description",
              UserSectionQuestionAndAnswer "Question?" "Answer!",
              UserSectionImages
                [ "https://via.placeholder.com/110",
                  "https://via.placeholder.com/120",
                  "https://via.placeholder.com/130"
                ]
                "Image description",
              UserSectionQuestionAndAnswer "Question?" "Answer!",
              UserSectionQuestionAndAnswer "Question?" "Answer!",
              UserSectionGeneric "Generic header" "Generic content",
              UserSectionImages
                [ "https://via.placeholder.com/110",
                  "https://via.placeholder.com/120",
                  "https://via.placeholder.com/130"
                ]
                "Image description",
              UserSectionGeneric "Generic header" "Generic content",
              UserSectionGeneric "Generic header" "Generic content"
            ],
          userExtRelationshipStatus = fromMaybe RelationshipStatusUnknown $ sqlToRelationshipStatus relationshipStatus,
          userExtImpression = impression
        }

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

createNewUser :: NewUserInfo -> Transaction UserID
createNewUser u = do
  row <-
    sqlTransaction
      [sql|
        INSERT INTO users (
          name,
          header_image_url,
          description,
          location,
          join_day,
          birthday,
          gender_identity,
          relationship_status,
          search_age_min,
          search_age_max,
          search_distance_km
        )
        VALUES (
          #{newUserName u},
          #{newUserHeaderImageUrl u},
          #{newUserDescription u},
          ll_to_earth(#{newUserLatitude u}, #{newUserLongitude u}),
          CURRENT_DATE,
          #{newUserBirthday u},
          #{newUserGenderIdentity u} :: gender_identity,
          'single',
          #{newUserSearchMinAge u},
          #{newUserSearchMaxAge u},
          #{newUserSearchDistanceM u} / 1000
        )
        RETURNING id
      |]
  let newID :: Int32
      newID = getOneColumn $ getOneRow row
  let newIDSql :: Sql
      newIDSql = [sql|#{newID}|]
  sqlTransaction_
    [sql|
      INSERT INTO user_search_gender_identities (
        user_id,
        search_gender_identity
      )
      SELECT ^{newIDSql}, *
      FROM UNNEST (#{newUserSearchGenderIdentities u} :: gender_identity[])
    |]
  pure $ UserID newID

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

userOverviewInfoFromSQL :: (Int32, Text, Text, Float, Day, Day, Maybe GenderIdentity, Float) -> UserOverviewInfo
userOverviewInfoFromSQL (uid, name, img, distanceM, curDate, birthday, genderId, searchDistanceM) =
  UserOverviewInfo
    { userId = UserID uid,
      userName = name,
      userHeaderImageUrl = img,
      userDistanceM = smartRoundDistanceM distanceM searchDistanceM,
      userAge = age curDate birthday,
      userGenderIdentity = fromMaybe Other genderId
    }

searchFor :: UserID -> Int32 -> Transaction [UserOverviewInfo]
searchFor userID maxResults = do
  rows <-
    sqlTransaction
      -- We can't do comments in the SQL here, because the parser doesn't support it.
      -- But essentially, we first just get all the tables we need (all the joins)
      -- then a block of queries to search people for me (the first block of `AND`'s),
      -- then the same but in reverse for reverse search
      -- and then lastly we filter out any users that you've already judged
      [sql|
        SELECT 
          other.id,
          other.name,
          other.header_image_url,
          earth_distance(me.location, other.location) :: float4,
          CURRENT_DATE,
          other.birthday,
          other.gender_identity,
          (other.search_distance_km * 1000) :: float4
        FROM users me
        JOIN users other ON other.id <> me.id
        JOIN user_search_gender_identities my_gi ON me.id = my_gi.user_id
        JOIN user_search_gender_identities other_gi ON other.id = other_gi.user_id
        WHERE me.id = #{userID}

        AND other.birthday
          BETWEEN CURRENT_DATE - concat(me.search_age_max::text,' years')::interval
          AND CURRENT_DATE - concat(me.search_age_min::text,' years')::interval
        AND other.gender_identity = my_gi.search_gender_identity
        AND earth_box(me.location, me.search_distance_km * 1000) @> other.location
        AND earth_distance(me.location, other.location) <= me.search_distance_km * 1000

        AND me.birthday
          BETWEEN CURRENT_DATE - concat(other.search_age_max::text,' years')::interval
          AND CURRENT_DATE - concat(other.search_age_min::text,' years')::interval
        AND me.gender_identity = other_gi.search_gender_identity
        AND earth_box(other.location, other.search_distance_km * 1000) @> me.location
        AND earth_distance(other.location, me.location) <= other.search_distance_km * 1000

        AND NOT EXISTS (
          SELECT 1
          FROM impressions
          WHERE user_id = me.id
          AND other_user_id = other.id
        )

        AND NOT EXISTS (
          SELECT 1
          FROM impressions
          WHERE user_id = other.id
          AND other_user_id = me.id
          AND impression = 'dislike'
        )

        LIMIT #{maxResults}
      |]
  pure $ userOverviewInfoFromSQL <$> rows

getUsersWithImpressionBy :: UserID -> Impression -> Transaction [UserOverviewInfo]
getUsersWithImpressionBy userID impression = do
  rows <-
    sqlTransaction
      [sql|
        SELECT 
          other.id,
          other.name,
          other.header_image_url,
          earth_distance(me.location, other.location),
          CURRENT_DATE,
          other.birthday,
          other.gender_identity,
          (other.search_distance_km * 1000)
        FROM users me
        JOIN users other ON other.id <> me.id
        JOIN impressions ON impressions.user_id = me.id AND impressions.other_user_id = other.id
        WHERE me.id = #{userID}
        AND impressions.impression = #{impression}::impression
        ORDER BY impressions.timestamp DESC
      |]
  pure $ userOverviewInfoFromSQL <$> rows

setImpressionBy :: UserID -> Impression -> UserID -> Transaction ()
setImpressionBy myID impression otherUserID =
  sqlTransaction
    [sql|
      INSERT LALALA impressions (
        user_id,
        impression,
        other_user_id,
        timestamp
      )
      VALUES (
        #{myID},
        #{impression},
        #{otherUserID},
        CURRENT_TIMESTAMP
      )
      ON CONFLICT (user_id, other_user_id) DO UPDATE SET 
        impression = #{impression}::impression,
        timestamp = CURRENT_TIMESTAMP
    |]
