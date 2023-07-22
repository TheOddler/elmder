{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Elm.Derive (deriveBoth)
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
  ( GenericMode ((:-)),
    Get,
    Handler,
    JSON,
    Post,
    ReqBody,
    (:>),
  )
import Servant.Elm qualified
import Servant.Server.Generic (AsServerT)

newtype UserID = UserID {unUserID :: Text}
  deriving (Generic)

data User = User
  { userID :: UserID,
    userName :: Text,
    userHeaderImage :: Text,
    userDescription :: Text,
    userRelationshipStatus :: RelationshipStatus,
    userSections :: [UserSection]
  }
  deriving (Generic)

data RelationshipStatus
  = RelationshipStatusSingle
  | RelationshipStatusMarried
  | RelationshipStatusInRelationship
  deriving (Generic, Show, Eq, Enum, Bounded)

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
deriveBoth Servant.Elm.defaultOptions ''UserSection
deriveBoth Servant.Elm.defaultOptions ''User

data UserRoutes mode = UserRoutes
  { getUsers :: mode :- "getMany" :> ReqBody '[JSON] [UserID] :> Post '[JSON] [User],
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