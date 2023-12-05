{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (CalendarDiffDays (..), Day, diffGregorianDurationClip)
import Data.Vector qualified as V
import Elm.Derive (Options (..), deriveBoth)
import GHC.Generics (Generic)
import Hasql.TH (resultlessStatement, singletonStatement, vectorStatement)
import Hasql.Transaction (Transaction, statement)
import SafeMath (monthsToYears, roundToNearest)
import Servant (FromHttpApiData, ToHttpApiData)
import Servant.Elm qualified
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
    userAge :: Integer,
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
    userExtRelationshipStatus :: RelationshipStatus -- TODO: Rework this to a web of relationships
  }
  deriving (Generic, Show, Eq)

data Location = Location
  { locationLatitude :: Float,
    locationLongitude :: Float
  }
  deriving (Generic, Show, Eq)

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

data RelationshipStatus
  = RelationshipStatusSingle
  | RelationshipStatusMarried
  | RelationshipStatusInRelationship
  deriving (Generic, Show, Eq, Enum, Bounded)

-- | Convert a 'RelationshipStatus' to a SQL name for it.
relationshipStatusToSQL :: RelationshipStatus -> Text
relationshipStatusToSQL = \case
  RelationshipStatusSingle -> "single"
  RelationshipStatusInRelationship -> "in_relationship"
  RelationshipStatusMarried -> "married"

sqlToRelationshipStatus :: Text -> Maybe RelationshipStatus
sqlToRelationshipStatus = reverseEnumToText relationshipStatusToSQL

data GenderIdentity
  = Abinary
  | Agender
  | Ambigender
  | Androgyne
  | Androgynous
  | Aporagender
  | Autigender
  | Bakla
  | Bigender
  | Binary
  | Bissu
  | Butch
  | Calabai
  | Calalai
  | CisFemale
  | CisMale
  | CisMan
  | CisWoman
  | DemiBoy
  | Demiflux
  | Demigender
  | DemiGirl
  | DemiGuy
  | DemiMan
  | DemiWoman
  | DualGender
  | FaAfafine
  | Female
  | FemaleToMale
  | Femme
  | FTM
  | GenderBender
  | GenderDiverse
  | GenderGifted
  | Genderfae
  | Genderfluid
  | Genderflux
  | Genderfuck
  | Genderless
  | GenderNonconforming
  | Genderqueer
  | GenderQuestioning
  | GenderVariant
  | Graygender
  | Hijra
  | Intergender
  | Intersex
  | Kathoey
  | Mahu
  | Male
  | MaleToFemale
  | Man
  | ManOfTransExperience
  | Maverique
  | MetaGender
  | MTF
  | Multigender
  | Muxe
  | Neither
  | Neurogender
  | Neutrois
  | NonBinary
  | NonBinaryTransgender
  | Omnigender
  | Other
  | Pangender
  | PersonOfTransgenderedExperience
  | Polygender
  | Sekhet
  | ThirdGender
  | TransFemale
  | TransMale
  | TransMan
  | TransPerson
  | TransWoman
  | TransgenderFemale
  | TransgenderMale
  | TransgenderMan
  | TransgenderPerson
  | TransgenderWoman
  | Transfeminine
  | Transmasculine
  | TranssexualFemale
  | TranssexualMale
  | TranssexualMan
  | TranssexualPerson
  | TranssexualWoman
  | Travesti
  | Trigender
  | Tumtum
  | TwoSpirit
  | Vakasalewalewa
  | Waria
  | Winkte
  | Woman
  | WomanOfTransExperience
  | XGender
  | XJenda
  | Xenogender
  deriving (Generic, Show, Eq, Enum, Bounded)

-- | Convert a 'RelationshipStatus' to a SQL name for it.
genderIdentityToSQL :: GenderIdentity -> Text
genderIdentityToSQL = \case
  Abinary -> "abinary"
  Agender -> "agender"
  Ambigender -> "ambigender"
  Androgyne -> "androgyne"
  Androgynous -> "androgynous"
  Aporagender -> "aporagender"
  Autigender -> "autigender"
  Bakla -> "bakla"
  Bigender -> "bigender"
  Binary -> "binary"
  Bissu -> "bissu"
  Butch -> "butch"
  Calabai -> "calabai"
  Calalai -> "calalai"
  CisFemale -> "cis_female"
  CisMale -> "cis_male"
  CisMan -> "cis_man"
  CisWoman -> "cis_woman"
  DemiBoy -> "demi_boy"
  Demiflux -> "demiflux"
  Demigender -> "demigender"
  DemiGirl -> "demi_girl"
  DemiGuy -> "demi_guy"
  DemiMan -> "demi_man"
  DemiWoman -> "demi_woman"
  DualGender -> "dual_gender"
  FaAfafine -> "fa_afafine"
  Female -> "female"
  FemaleToMale -> "female_to_male"
  Femme -> "femme"
  FTM -> "ftm"
  GenderBender -> "gender_bender"
  GenderDiverse -> "gender_diverse"
  GenderGifted -> "gender_gifted"
  Genderfae -> "genderfae"
  Genderfluid -> "genderfluid"
  Genderflux -> "genderflux"
  Genderfuck -> "genderfuck"
  Genderless -> "genderless"
  GenderNonconforming -> "gender_nonconforming"
  Genderqueer -> "genderqueer"
  GenderQuestioning -> "gender_questioning"
  GenderVariant -> "gender_variant"
  Graygender -> "graygender"
  Hijra -> "hijra"
  Intergender -> "intergender"
  Intersex -> "intersex"
  Kathoey -> "kathoey"
  Mahu -> "mahu"
  Male -> "male"
  MaleToFemale -> "male_to_female"
  Man -> "man"
  ManOfTransExperience -> "man_of_trans_experience"
  Maverique -> "maverique"
  MetaGender -> "meta_gender"
  MTF -> "mtf"
  Multigender -> "multigender"
  Muxe -> "muxe"
  Neither -> "neither"
  Neurogender -> "neurogender"
  Neutrois -> "neutrois"
  NonBinary -> "non_binary"
  NonBinaryTransgender -> "non_binary_transgender"
  Omnigender -> "omnigender"
  Other -> "other"
  Pangender -> "pangender"
  PersonOfTransgenderedExperience -> "person_of_transgendered_experience"
  Polygender -> "polygender"
  Sekhet -> "sekhet"
  ThirdGender -> "third_gender"
  TransFemale -> "trans_female"
  TransMale -> "trans_male"
  TransMan -> "trans_man"
  TransPerson -> "trans_person"
  TransWoman -> "trans_woman"
  TransgenderFemale -> "transgender_female"
  TransgenderMale -> "transgender_male"
  TransgenderMan -> "transgender_man"
  TransgenderPerson -> "transgender_person"
  TransgenderWoman -> "transgender_woman"
  Transfeminine -> "transfeminine"
  Transmasculine -> "transmasculine"
  TranssexualFemale -> "transsexual_female"
  TranssexualMale -> "transsexual_male"
  TranssexualMan -> "transsexual_man"
  TranssexualPerson -> "transsexual_person"
  TranssexualWoman -> "transsexual_woman"
  Travesti -> "travesti"
  Trigender -> "trigender"
  Tumtum -> "tumtum"
  TwoSpirit -> "two_spirit"
  Vakasalewalewa -> "vakasalewalewa"
  Waria -> "waria"
  Winkte -> "winkte"
  Woman -> "woman"
  WomanOfTransExperience -> "woman_of_trans_experience"
  XGender -> "x_gender"
  XJenda -> "x_jenda"
  Xenogender -> "xenogender"

sqlToGenderIdentity :: Text -> Maybe GenderIdentity
sqlToGenderIdentity = reverseEnumToText genderIdentityToSQL

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

data SearchParameters = SearchParameters
  { searchAgeMin :: Int,
    searchAgeMax :: Int,
    searchDistanceKm :: Float,
    searchGenderIdentity :: [GenderIdentity]
  }
  deriving (Generic)

-- Use the default options from Servant.Elm, they work best for Elm, not those from Elm.Derive nor Aeson.
deriveBoth Servant.Elm.defaultOptions {unwrapUnaryRecords = True} ''UserID
deriveBoth Servant.Elm.defaultOptions ''RelationshipStatus
deriveBoth Servant.Elm.defaultOptions ''GenderIdentity
deriveBoth Servant.Elm.defaultOptions ''Location
deriveBoth Servant.Elm.defaultOptions ''ProfileSection
deriveBoth Servant.Elm.defaultOptions ''UserOverviewInfo
deriveBoth Servant.Elm.defaultOptions ''UserExtendedInfo

getUserExtendedInfo :: UserID -> Transaction UserExtendedInfo
getUserExtendedInfo _ =
  pure
    UserExtendedInfo
      { -- TODO: Implement actually getting this stuff from the DB
        userExtProfileAge = 0,
        userExtDescription = "This is a test description",
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
            UserSectionQuestionAndAnswer "Question?" "Answer!"
          ],
        userExtRelationshipStatus = RelationshipStatusSingle
      }

data NewUserInfo = NewUserInfo
  { newUserName :: Text,
    newUserDescription :: Text,
    newUserHeaderImageUrl :: Text,
    newUserLocation :: Location,
    newUserBirthday :: Day,
    newUserGenderIdentity :: GenderIdentity
  }

createNewUser :: NewUserInfo -> Transaction UserID
createNewUser u = do
  newID <-
    statement
      ( u.newUserName,
        u.newUserHeaderImageUrl,
        u.newUserDescription,
        u.newUserLocation.locationLatitude,
        u.newUserLocation.locationLongitude,
        u.newUserBirthday,
        genderIdentityToSQL u.newUserGenderIdentity
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
              18,
              99,
              1000
            )
            RETURNING id :: int
          |]
  statement
    (newID, V.fromList $ genderIdentityToSQL <$> [minBound .. maxBound])
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
      [vectorStatement|
        SELECT 
          other.id :: int,
          other.name :: text,
          other.header_image_url :: text,
          earth_distance(me.location, other.location) :: float,
          CURRENT_DATE :: date,
          other.birthday :: date,
          other.gender_identity :: text,
          (other.search_distance_km * 1000) :: float
        FROM users me
        JOIN users other ON other.id <> me.id
        JOIN user_search_gender_identities my_gi ON me.id = my_gi.user_id
        JOIN user_search_gender_identities other_gi ON other.id = other_gi.user_id
        WHERE me.id = $1 :: int

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

        LIMIT $2 :: int
      |]
  pure $ toUserInfo <$> toList rows
  where
    toUserInfo (uid, name, img, distanceM, curDate, birthday, genderId, searchDistanceM) =
      UserOverviewInfo
        { userId = UserID uid,
          userName = name,
          userHeaderImageUrl = img,
          userDistanceM = smartRoundDistanceM distanceM searchDistanceM,
          userAge = monthsToYears $ cdMonths (diffGregorianDurationClip curDate birthday),
          userGenderIdentity = fromMaybe Other $ sqlToGenderIdentity genderId
        }
