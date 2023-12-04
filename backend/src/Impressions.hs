{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Impressions where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (cdMonths, diffGregorianDurationClip)
import Data.Vector (toList)
import Elm.Derive (deriveBoth)
import Hasql.TH (resultlessStatement, vectorStatement)
import Hasql.Transaction (Transaction, statement)
import SafeMath (monthsToYears)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Elm qualified
import User (GenderIdentity (..), Location (..), UserID (..), UserOverviewInfo (..), distanceInMBetween, sqlToGenderIdentity)
import Util (reverseEnumToText)

-- | Note that the below descriptions are just ideas, not all of it is implemented yet
data Impression
  = -- | The user liked the other user, if they like each other it's a match
    ImpressionLike
  | -- | The user disliked the other user, we will not show them to each other again
    ImpressionDislike
  | -- | The user is undecided, we won't show the user to them any more, but the other user can still see them
    ImpressionDecideLater
  | -- | The user REALLY likes the other user, the other user will get a notification about this
    ImpressionSuperLike
  deriving (Eq, Show, Enum, Bounded)

deriveBoth Servant.Elm.defaultOptions ''Impression

instance ToHttpApiData Impression where
  toUrlPiece = impressionToSQL

instance FromHttpApiData Impression where
  parseUrlPiece t = case reverseEnumToText toUrlPiece t of
    Nothing -> Left $ "Invalid impression: " <> t
    Just x -> Right x

-- | Convert a 'RelationshipStatus' to a SQL name for it.
impressionToSQL :: Impression -> Text
impressionToSQL = \case
  ImpressionLike -> "like"
  ImpressionDislike -> "dislike"
  ImpressionDecideLater -> "decide_later"
  ImpressionSuperLike -> "super_like"

sqlToImpression :: Text -> Maybe Impression
sqlToImpression = reverseEnumToText impressionToSQL

getImpressionBy :: UserID -> Impression -> Transaction [UserOverviewInfo]
getImpressionBy userID impression = do
  rows <-
    statement
      (unUserID userID, impressionToSQL impression)
      [vectorStatement|
        SELECT 
          other.id :: int,
          other.name :: text,
          other.header_image_url :: text,
          other.description :: text,
          me.last_location_lat :: float,
          me.last_location_long :: float,
          other.last_location_lat :: float,
          other.last_location_long :: float,
          CURRENT_DATE :: date,
          other.birthday :: date,
          other.gender_identity :: text,
          other.search_distance_km :: float
        FROM users me
        JOIN users other ON other.id <> me.id
        JOIN impressions ON impressions.user_id = me.id AND impressions.other_user_id = other.id
        WHERE me.id = $1 :: int
        AND impressions.impression = $2 :: text :: impression
        ORDER BY impressions.timestamp DESC
      |]
  pure $ toUserInfo <$> toList rows
  where
    toUserInfo (uid, name, img, descr, myLat, myLong, otherLat, otherLong, curDate, birthday, genderId, search_distance) =
      UserOverviewInfo
        { userId = UserID uid,
          userName = name,
          userHeaderImageUrl = img,
          userDescription = descr,
          userDistanceM = distanceInMBetween (Location myLat myLong) (Location otherLat otherLong) search_distance,
          userAge = monthsToYears $ cdMonths (diffGregorianDurationClip curDate birthday),
          userGenderIdentity = fromMaybe Other $ sqlToGenderIdentity genderId
        }

setImpressionBy :: UserID -> Impression -> UserID -> Transaction ()
setImpressionBy userID impression otherUserID =
  statement
    (unUserID userID, impressionToSQL impression, unUserID otherUserID)
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
