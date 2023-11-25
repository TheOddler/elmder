{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import DB (runHasql)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day)
import Data.Vector (fromList)
import Elm.Derive (deriveBoth)
import GHC.Generics (Generic)
import Hasql.TH (vectorStatement)
import Servant.Elm qualified
import ServerM (ServerM)
import Util (reverseEnumToText)

newtype UserID = UserID {unUserID :: Int32}
  deriving (Generic)

data User = User
  { userID :: UserID,
    userName :: Text,
    userHeaderImage :: Text,
    userLastKnownLocation :: Location,
    userJoinDay :: Day,
    userBirthday :: Day,
    userGenderIdentity :: GenderIdentity,
    userDescription :: Text,
    userRelationshipStatus :: RelationshipStatus,
    userSections :: [UserSection]
  }
  deriving (Generic)

data Location = Location
  { locationLatitude :: Float,
    locationLongitude :: Float
  }
  deriving (Generic)

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

data UserSection
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
  deriving (Generic)

-- Use the default options from Servant.Elm, they work best for Elm, not those from Elm.Derive nor Aeson.
deriveBoth Servant.Elm.defaultOptions ''UserID
deriveBoth Servant.Elm.defaultOptions ''RelationshipStatus
deriveBoth Servant.Elm.defaultOptions ''GenderIdentity
deriveBoth Servant.Elm.defaultOptions ''Location
deriveBoth Servant.Elm.defaultOptions ''UserSection
deriveBoth Servant.Elm.defaultOptions ''User

getUsers :: [UserID] -> ServerM [User]
getUsers ids = do
  rows <-
    runHasql
      (fromList $ unUserID <$> ids)
      [vectorStatement|
        SELECT
          id :: int,
          name :: text,
          header_image_url :: text,
          last_location_lat :: float4,
          last_location_long :: float4,
          join_day :: date,
          birthday :: date,
          gender_identity :: text,
          description :: text,
          relationship_status :: text
        FROM users
        WHERE id = ANY ($1 :: int[])
      |]
  pure $ toUser <$> toList rows
  where
    toUser (userId, name, img, lat, long, joinDay, birthday, genderIdentity, descr, status) =
      User
        { userID = UserID userId,
          userName = name,
          userHeaderImage = img,
          userLastKnownLocation = Location lat long,
          userJoinDay = joinDay,
          userBirthday = birthday,
          userGenderIdentity =
            fromMaybe Other $
              sqlToGenderIdentity genderIdentity,
          userDescription = descr,
          userRelationshipStatus =
            fromMaybe RelationshipStatusSingle $ sqlToRelationshipStatus status,
          userSections = []
        }
