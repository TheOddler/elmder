{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module User where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Faker (Fake, generateNonDeterministic)
import Faker.Book.Lovecraft qualified
import Faker.Combinators (fakeBoundedEnum, fromRange, listOf, oneof)
import Faker.Creature.Cat qualified
import Faker.Food qualified
import Faker.FunnyName qualified
import Faker.Name qualified
import Faker.Superhero qualified
import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic (AsServerT)

newtype UserID = UserID {unUserID :: Text}
  deriving (Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToParamSchema)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec UserID

instance ToHttpApiData [UserID] where
  toUrlPiece :: [UserID] -> Text
  toUrlPiece ids = T.intercalate "," $ unUserID <$> ids

instance FromHttpApiData [UserID] where
  parseUrlPiece :: Text -> Either Text [UserID]
  parseUrlPiece = traverse parseUrlPiece . T.split (== ',')

instance HasCodec UserID where
  codec = named "UserID" $ dimapCodec UserID unUserID codec

data User = User
  { userID :: UserID,
    userName :: Text,
    userHeaderImage :: Text,
    userDescription :: Text,
    userRelationshipStatus :: RelationshipStatus,
    userSections :: [UserSection]
  }
  deriving (Generic)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec User

instance HasCodec User where
  codec =
    object "User" $
      User
        <$> requiredField "id" "The user's unique ID"
        .= userID
        <*> requiredField "name" "The users's name"
        .= userName
        <*> requiredField "headerImage" "A url to the user's primary image"
        .= userHeaderImage
        <*> requiredField "description" "The user's profile description"
        .= userDescription
        <*> requiredField "relationshipStatus" "The user's relationship status"
        .= userRelationshipStatus
        <*> requiredField "sections" "Different sections of the user's profile"
        .= userSections

data RelationshipStatus
  = RelationshipStatusSingle
  | RelationshipStatusMarried
  | RelationshipStatusInRelationship
  deriving (Generic, Show, Eq, Enum, Bounded)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec RelationshipStatus

instance HasCodec RelationshipStatus where
  codec =
    let allStatuses = minBound NE.:| [succ minBound .. maxBound]
        encode = \case
          RelationshipStatusSingle -> "single"
          RelationshipStatusMarried -> "married"
          RelationshipStatusInRelationship -> "in-relationship"
     in stringConstCodec (NE.map (\v -> (v, encode v)) allStatuses)

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
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec UserSection

instance HasCodec UserSection where
  codec = object "UserSection" $ discriminatedUnionCodec "tag" enc dec
    where
      enc :: UserSection -> (Discriminator, ObjectCodec UserSection ())
      enc section =
        case section of
          UserSectionGeneric _ _ -> ("generic", mapToEncoder section generic)
          UserSectionImages _ _ -> ("images", mapToEncoder section images)
          UserSectionQuestionAndAnswer _ _ -> ("qna", mapToEncoder section questionAndAnswer)

      dec :: HashMap.HashMap Discriminator (Text, ObjectCodec Void UserSection)
      dec =
        HashMap.fromList
          [ ("generic", ("UserSectionGeneric", mapToDecoder id generic)),
            ("images", ("UserSectionImages", mapToDecoder id images)),
            ("qna", ("UserSectionQuestionAndAnswer", mapToDecoder id questionAndAnswer))
          ]

      generic =
        UserSectionGeneric
          <$> requiredField "header" "The header for this section"
          .= userSectionGenericHeader
          <*> requiredField "content" "The content of this section"
          .= userSectionGenericContent

      images =
        UserSectionImages
          <$> requiredField "images" "A list of image urls"
          .= userSectionImagesImages
          <*> requiredField "description" "De description for all images together"
          .= userSectionImagesDescription

      questionAndAnswer =
        UserSectionQuestionAndAnswer
          <$> requiredField "question" "The question"
          .= userSectionQuestionAndAnswerQuestion
          <*> requiredField "answer" "The answer"
          .= userSectionQuestionAndAnswerAnswer

data UserRoutes mode = UserRoutes
  { getUsers :: mode :- "many" :> Capture "id" [UserID] :> Get '[JSON] [User],
    getSomeUserIDs :: mode :- "exampleIDs" :> Get '[JSON] [UserID]
  }
  deriving (Generic)

userRoutes :: UserRoutes (AsServerT Handler)
userRoutes = UserRoutes {..}
  where
    getUsers = getUsersHandler
    getSomeUserIDs = pure $ UserID . T.pack . show <$> [1 .. 10 :: Int]

getUsersHandler :: [UserID] -> Handler [User]
getUsersHandler ids = liftIO $ generateNonDeterministic $ mapM fakeUser ids
  where
    maybeF :: Fake a -> Fake (Maybe a)
    maybeF f = oneof [pure Nothing, Just <$> f]

    fakeImgUrl :: Fake Text
    fakeImgUrl = do
      imgWidth <- fromRange (200, 600 :: Int)
      imgHeight <- fromRange (200, 600 :: Int)
      pure $ "https://placekitten.com/" <> T.pack (show imgWidth) <> "/" <> T.pack (show imgHeight)

    fakeUserSection :: Fake UserSection
    fakeUserSection = do
      oneof
        [ do
            userSectionGenericHeader <- Faker.Food.dish
            userSectionGenericContent <- Faker.Food.descriptions
            pure UserSectionGeneric {..},
          do
            imageCount <- fromRange (1, 10 :: Int)
            userSectionImagesImages <- listOf imageCount fakeImgUrl
            userSectionImagesDescription <- Faker.Superhero.descriptor
            pure UserSectionImages {..},
          do
            userSectionQuestionAndAnswerQuestion <- Faker.Food.dish
            userSectionQuestionAndAnswerAnswer <- Faker.Food.descriptions
            pure UserSectionQuestionAndAnswer {..}
        ]

    fakeUser :: UserID -> Fake User
    fakeUser userID = do
      userName <- do
        T.unwords
          . catMaybes
          <$> sequence
            [ maybeF Faker.Name.prefix,
              Just <$> Faker.Creature.Cat.name,
              Just <$> Faker.FunnyName.name
            ]

      userHeaderImage <- fakeImgUrl

      descriptionLength <- fromRange (4, 15 :: Int)
      userDescription <- T.unwords <$> listOf descriptionLength Faker.Book.Lovecraft.words

      userRelationshipStatus <- fakeBoundedEnum

      sectionsCount <- fromRange (3, 15 :: Int)
      userSections <- listOf sectionsCount fakeUserSection

      pure User {..}