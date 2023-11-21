{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module User.Fake where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import DB (runHasql)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (fromList)
import Data.Vector qualified as V
import Faker (Fake, generateNonDeterministic)
import Faker.Book.Lovecraft qualified
import Faker.Combinators (fakeBoundedEnum, fromRange, listOf, oneof)
import Faker.Creature.Cat qualified
import Faker.Food qualified
import Faker.FunnyName qualified
import Faker.Name qualified
import Faker.Superhero qualified
import Hasql.TH (resultlessStatement, singletonStatement)
import ServerM (ServerM)
import User

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

ensureSomeUsersInDB :: Int32 -> ServerM ()
ensureSomeUsersInDB wanted = do
  currentCount <- runHasql () [singletonStatement| SELECT COUNT(*) :: int FROM users |]
  let needed = wanted - currentCount
  when (needed > 0) $ do
    someUsers <- liftIO $ generateNonDeterministic $ mapM fakeUser (UserID <$> [1 .. needed]) -- ids are not actually used to insert, so it's fine to use the same ids every time
    runHasql
      (V.unzip4 $ fromList $ fromUser <$> someUsers)
      [resultlessStatement|
        INSERT INTO users (name, header_image_url, description, relationship_status)
        SELECT *
        FROM UNNEST (
          $1 :: text[],
          $2 :: text[],
          $3 :: text[],
          $4 :: text[] :: relationship_status[]
        )
      |]
  where
    fromUser user =
      ( user.userName,
        user.userHeaderImage,
        user.userDescription,
        relationshipStatusToSQL user.userRelationshipStatus
      )
