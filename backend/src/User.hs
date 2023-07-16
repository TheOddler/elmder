{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
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
import Servant.Elm
import Servant.Server.Generic (AsServerT)

newtype UserID = UserID {unUserID :: Text}
  deriving (Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance ToHttpApiData [UserID] where
  toUrlPiece :: [UserID] -> Text
  toUrlPiece ids = T.intercalate "," $ unUserID <$> ids

instance FromHttpApiData [UserID] where
  parseUrlPiece :: Text -> Either Text [UserID]
  parseUrlPiece = traverse parseUrlPiece . T.split (== ',')

data User = User
  { userID :: UserID,
    userName :: Text,
    userHeaderImage :: Text,
    userDescription :: Text,
    relationshipStatus :: RelationshipStatus,
    userSections :: [UserSection]
  }
  deriving (Generic)

data RelationshipStatus
  = RelationshipStatusSingle
  | RelationshipStatusMarried
  | RelationshipStatusInRelationship
  deriving (Generic, Enum, Bounded)

data UserSection
  = UserSectionGeneric {header :: Text, content :: Text}
  | UserSectionImages {images :: [Text], description :: Text}
  | UserSectionQuestionAndAnswer {question :: Text, answer :: Text}
  deriving (Generic)

data UserRoutes mode = UserRoutes
  { getUsers :: mode :- "many" :> Capture "id" [UserID] :> Get '[JSON] [User],
    getSomeUserIDs :: mode :- "exampleIDs" :> Get '[JSON] [UserID]
  }
  deriving (Generic)

deriveBoth defaultOptions ''UserID
deriveBoth defaultOptions ''RelationshipStatus
deriveBoth defaultOptions ''UserSection
deriveBoth defaultOptions ''User

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
            header <- Faker.Food.dish
            content <- Faker.Food.descriptions
            pure UserSectionGeneric {..},
          do
            imageCount <- fromRange (1, 10 :: Int)
            images <- listOf imageCount fakeImgUrl
            description <- Faker.Superhero.descriptor
            pure UserSectionImages {..},
          do
            question <- Faker.Food.dish
            answer <- Faker.Food.descriptions
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

      relationshipStatus <- fakeBoundedEnum

      sectionsCount <- fromRange (3, 15 :: Int)
      userSections <- listOf sectionsCount fakeUserSection

      pure User {..}