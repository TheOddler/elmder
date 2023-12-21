{-# LANGUAGE TemplateHaskell #-}

module User.GenderIdentity where

import Data.Text (Text)
import DeriveElmAndJson (deriveElmAndJson)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Util (reverseEnumToText)

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

instance EncodeValue GenderIdentity where
  encodeValue = Encoders.enum genderIdentityToSQL

instance DecodeValue GenderIdentity where
  decodeValue = Decoders.enum $ reverseEnumToText genderIdentityToSQL

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

deriveElmAndJson ''GenderIdentity
