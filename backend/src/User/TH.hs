module User.TH where

import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day)
import Hasql.TH (singletonStatement, vectorStatement)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import SafeMaths (age)
import User (UserID (..), UserOverviewInfo (..), smartRoundDistanceM)
import User.GenderIdentity (GenderIdentity (..), genderIdentityFromSQL)
import User.Impressions (impressionFromSQL)

-- | A quasi-quoter for 'UserOverviewInfo'.
-- It takes a partial SQL query and creates a query that you can decode
-- with the `userOverviewInfoDecoder` function.
-- The partial SQL it can accept can be some additional JOIN statements, and
-- the WHERE clause.
-- This is essentially `singletonStatement` from Hasql but with a predefined
-- select statement.
userOverviewInfoStatement :: QuasiQuoter
userOverviewInfoStatement =
  QuasiQuoter
    { quoteExp = \wherePart -> quoteExp singletonStatement $ userOverviewInfoSelect ++ wherePart,
      quotePat = unsupported,
      quoteType = unsupported,
      quoteDec = unsupported
    }
  where
    unsupported _ = fail "Unsupported"

-- | Similar to `userOverviewInfoQQ` but returns multiple rows.
-- This is essentially `vectorStatement` from Hasql but with a predefined
-- select statement.
usersOverviewInfoStatement :: QuasiQuoter
usersOverviewInfoStatement =
  QuasiQuoter
    { quoteExp = \wherePart -> quoteExp vectorStatement $ userOverviewInfoSelect ++ wherePart,
      quotePat = unsupported,
      quoteType = unsupported,
      quoteDec = unsupported
    }
  where
    unsupported _ = fail "Unsupported"

userOverviewInfoSelect :: String
userOverviewInfoSelect =
  "SELECT\n\
  \  other.id :: int,\n\
  \  other.name :: text,\n\
  \  other.header_image_url :: text,\n\
  \  earth_distance(me.location, other.location) :: float4,\n\
  \  CURRENT_DATE :: date,\n\
  \  other.birthday :: date,\n\
  \  other.gender_identity :: text,\n\
  \  my_imp.impression :: text?,\n\
  \  (other.search_distance_km * 1000) :: float4\n\
  \FROM users me\n\
  \CROSS JOIN users other\n\
  \LEFT JOIN impressions my_imp ON my_imp.user_id = me.id AND my_imp.other_user_id = other.id\n\
  \" -- End with a newline

userOverviewInfoDecoder :: (Int32, Text, Text, Float, Day, Day, Text, Maybe Text, Float) -> UserOverviewInfo
userOverviewInfoDecoder (uid, name, img, distanceM, curDate, birthday, genderId, impression, searchDistanceM) =
  UserOverviewInfo
    { userId = UserID uid,
      userName = name,
      userHeaderImageUrl = img,
      userDistanceM = smartRoundDistanceM distanceM searchDistanceM,
      userAge = age curDate birthday,
      userGenderIdentity = fromMaybe Other $ genderIdentityFromSQL genderId,
      userImpression = impressionFromSQL =<< impression
    }
