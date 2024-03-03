{-# LANGUAGE QuasiQuotes #-}

module User.Queries where

import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day)
import Data.Vector qualified as V
import Hasql.TH (resultlessStatement, singletonStatement)
import Hasql.Transaction (Transaction, statement)
import SafeMaths (age)
import User (NewUserInfo (..), ProfileSection (..), RelationshipStatus (..), UserExtendedInfo (..), UserID (..), UserOverviewInfo, sqlToRelationshipStatus)
import User.GenderIdentity (genderIdentityToSQL)
import User.Impressions (Impression (..), impressionToSQL)
import User.TH (userOverviewInfoDecoder, userOverviewInfoStatement, usersOverviewInfoStatement)

-- | Get the information about a different user
-- Since some of this info depends on the currently logged in user, such as the distance to them, we need to pass that in too.
getUserInfo :: UserID -> UserID -> Transaction UserOverviewInfo
getUserInfo myID otherID = do
  rows <-
    statement
      (unUserID myID, unUserID otherID)
      [userOverviewInfoStatement|
        WHERE me.id = $1 :: int
        AND other.id = $2 :: int
      |]
  pure $ userOverviewInfoDecoder rows

getUserExtendedInfo :: UserID -> UserID -> Transaction UserExtendedInfo
getUserExtendedInfo myID otherID = do
  rows <-
    statement
      (unUserID myID, unUserID otherID)
      [singletonStatement|
        SELECT 
          join_day :: date,
          CURRENT_DATE :: date,
          description :: text,
          relationship_status :: text
        FROM users
        LEFT JOIN impressions ON impressions.user_id = $1 :: int AND impressions.other_user_id = $2 :: int
        WHERE id = $2 :: int
      |]
  pure $ fromSQL rows
  where
    fromSQL :: (Day, Day, Text, Text) -> UserExtendedInfo
    fromSQL (profileDate, currentDate, description, relationshipStatus) =
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
          userExtRelationshipStatus = fromMaybe RelationshipStatusUnknown $ sqlToRelationshipStatus relationshipStatus
        }

createNewUser :: NewUserInfo -> Transaction UserID
createNewUser u = do
  newID <-
    statement
      ( u.newUserName,
        u.newUserHeaderImageUrl,
        u.newUserDescription,
        u.newUserLatitude,
        u.newUserLongitude,
        u.newUserBirthday,
        genderIdentityToSQL u.newUserGenderIdentity,
        u.newUserSearchMinAge,
        u.newUserSearchMaxAge,
        u.newUserSearchDistanceM
      )
      [singletonStatement|
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
              $1 :: text,
              $2 :: text,
              $3 :: text,
              ll_to_earth($4 :: float4, $5 :: float4),
              CURRENT_DATE,
              $6 :: date,
              $7 :: text :: gender_identity,
              'single',
              $8 :: int,
              $9 :: int,
              $10 :: int4 / 1000
            )
            RETURNING id :: int
          |]
  statement
    (newID, V.fromList $ genderIdentityToSQL <$> u.newUserSearchGenderIdentities)
    [resultlessStatement|
          INSERT INTO user_search_gender_identities (
            user_id,
            search_gender_identity
          )
          SELECT $1 :: int, *
          FROM UNNEST ($2 :: text[] :: gender_identity[])
        |]
  pure $ UserID newID

searchFor :: UserID -> Int32 -> Transaction [UserOverviewInfo]
searchFor userID maxResults = do
  rows <-
    statement
      (unUserID userID, maxResults)
      -- We can't do comments in the SQL here, because the parser doesn't support it.
      -- But essentially, we first just get all the tables we need (all the joins)
      -- then a block of queries to search people for me (the first block of `AND`'s),
      -- then the same but in reverse for reverse search
      -- and then lastly we filter out any users that you've already judged
      [usersOverviewInfoStatement|
        JOIN user_search_gender_identities my_gi ON me.id = my_gi.user_id
        JOIN user_search_gender_identities other_gi ON other.id = other_gi.user_id
        WHERE me.id = $1 :: int
        AND other.id <> me.id

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

        AND my_imp.impression IS NULL

        AND NOT EXISTS (
          SELECT 1
          FROM impressions
          WHERE user_id = other.id
          AND other_user_id = me.id
          AND impression = 'dislike'
        )

        LIMIT $2 :: int
      |]
  pure $ userOverviewInfoDecoder <$> toList rows

getUsersWithImpressionBy :: UserID -> [Impression] -> Transaction [UserOverviewInfo]
getUsersWithImpressionBy userID impressions = do
  rows <-
    statement
      (unUserID userID, V.fromList $ impressionToSQL <$> impressions)
      [usersOverviewInfoStatement|
        WHERE me.id = $1 :: int
        AND other.id <> me.id
        AND my_imp.impression = ANY ($2 :: text[] :: impression[])
        ORDER BY my_imp.timestamp DESC
      |]
  pure $ userOverviewInfoDecoder <$> toList rows

getMatchesFor :: UserID -> Transaction [UserOverviewInfo]
getMatchesFor userID = do
  rows <-
    statement
      (unUserID userID)
      [usersOverviewInfoStatement|
        JOIN impressions other_imp ON other_imp.user_id = other.id AND other_imp.other_user_id = me.id
        WHERE me.id = $1 :: int
        AND other.id <> me.id
        AND my_imp.impression IN ('like', 'super_like')
        AND other_imp.impression IN ('like', 'super_like')
        ORDER BY my_imp.timestamp DESC
      |]
  pure $ userOverviewInfoDecoder <$> toList rows

getUsersThatLike :: UserID -> Transaction [UserOverviewInfo]
getUsersThatLike userID = do
  rows <-
    statement
      (unUserID userID)
      [usersOverviewInfoStatement|
        JOIN impressions other_imp ON other_imp.user_id = other.id AND other_imp.other_user_id = me.id
        WHERE me.id = $1 :: int
        AND other.id <> me.id
        AND other_imp.impression IN ('like', 'super_like')
        ORDER BY other_imp.timestamp DESC
      |]
  pure $ userOverviewInfoDecoder <$> toList rows

setImpressionBy :: UserID -> Impression -> UserID -> Transaction ()
setImpressionBy myID impression otherUserID =
  statement
    (unUserID myID, impressionToSQL impression, unUserID otherUserID)
    [resultlessStatement|
      INSERT INTO impressions (
        user_id,
        impression,
        other_user_id,
        timestamp
      )
      VALUES (
        $1 :: int,
        $2 :: text :: impression,
        $3 :: int,
        CURRENT_TIMESTAMP
      )
      ON CONFLICT (user_id, other_user_id) DO UPDATE SET 
        impression = $2 :: text :: impression,
        timestamp = CURRENT_TIMESTAMP
    |]
