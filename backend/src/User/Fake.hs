{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module User.Fake where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import DB (runHasql)
import Data.Int (Int32)
import Data.List (genericTake)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Faker (Fake, generateNonDeterministic)
import Faker.Book.Lovecraft qualified
import Faker.Combinators (fakeBoundedEnum, fromRange, listOf, oneof)
import Faker.Creature.Cat qualified
import Faker.DateTime qualified
import Faker.Food qualified
import Faker.FunnyName qualified
import Faker.Name qualified
import Faker.Superhero qualified
import Hasql.TH (resultlessStatement, singletonStatement, vectorStatement)
import Hasql.Transaction (statement)
import ServerM (ServerM)
import User

maybeF :: Fake a -> Fake (Maybe a)
maybeF f = oneof [pure Nothing, Just <$> f]

fakeImgUrl :: Fake Text
fakeImgUrl = do
  imgWidth <- fromRange (200, 600 :: Int)
  imgHeight <- fromRange (200, 600 :: Int)
  pure $ "https://placekitten.com/" <> T.pack (show imgWidth) <> "/" <> T.pack (show imgHeight)

fakeUserSection :: Fake ProfileSection
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

  -- Place users close to each other so they can find each other
  -- Belgium: lat 50.503887 and long 4.469936
  userLastKnownLocation <- Location <$> Faker.Combinators.fromRange (49.0, 51.0) <*> Faker.Combinators.fromRange (3.0, 5.0)

  userBirthday <- Faker.DateTime.dayBetweenYears 1950 2000
  userJoinDay <- Faker.DateTime.dayBetweenYears 2020 2025

  userGenderIdentity <- fakeBoundedEnum

  pure User {..}

ensureSomeUsersInDB :: Int32 -> ServerM ()
ensureSomeUsersInDB wanted = do
  -- We generate definitely enough users, but might not insert all
  -- We generate these here first, so we can do the check and insert queries as a single transaction
  fakeUsers <- liftIO $ generateNonDeterministic $ mapM fakeUser (UserID <$> [1 .. wanted]) -- ids are not actually used to insert, so it's fine to use the same ids every time
  runHasql $ do
    currentCount <- statement () [singletonStatement| SELECT COUNT(*) :: int FROM users |]
    let usersToAdd = genericTake (wanted - currentCount) fakeUsers
    when (not $ null usersToAdd) $ do
      ids <-
        statement
          (unzip9 . V.fromList $ fromUser <$> usersToAdd)
          [vectorStatement|
            INSERT INTO users (
              name,
              header_image_url,
              last_location_lat,
              last_location_long,
              join_day,
              birthday,
              gender_identity,
              description,
              relationship_status,
              search_age_min,
              search_age_max,
              search_distance_km
            )
            SELECT *, 18, 99, 1000
            FROM UNNEST (
              $1 :: text[],
              $2 :: text[],
              $3 :: float4[],
              $4 :: float4[],
              $5 :: date[],
              $6 :: date[],
              $7 :: text[] :: gender_identity[],
              $8 :: text[],
              $9 :: text[] :: relationship_status[]
            )
            RETURNING id :: int
          |]
      statement
        (ids, V.fromList $ genderIdentityToSQL <$> [minBound .. maxBound])
        [resultlessStatement|
          INSERT INTO user_search_gender_identities (
            user_id,
            search_gender_identity
          )
          SELECT ids.*, gi.*
          FROM UNNEST ($1 :: int[]) ids
          CROSS JOIN UNNEST ($2 :: text[] :: gender_identity[]) gi
        |]
  where
    fromUser :: User -> (Text, Text, Float, Float, Day, Day, Text, Text, Text)
    fromUser user =
      ( user.userName,
        user.userHeaderImage,
        user.userLastKnownLocation.locationLatitude,
        user.userLastKnownLocation.locationLongitude,
        user.userJoinDay,
        user.userBirthday,
        genderIdentityToSQL user.userGenderIdentity,
        user.userDescription,
        relationshipStatusToSQL user.userRelationshipStatus
      )

    unzip9 :: Vector (a, b, c, d, e, f, g, h, i) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector i)
    unzip9 xs =
      ( fmap (\(a, _, _, _, _, _, _, _, _) -> a) xs,
        fmap (\(_, b, _, _, _, _, _, _, _) -> b) xs,
        fmap (\(_, _, c, _, _, _, _, _, _) -> c) xs,
        fmap (\(_, _, _, d, _, _, _, _, _) -> d) xs,
        fmap (\(_, _, _, _, e, _, _, _, _) -> e) xs,
        fmap (\(_, _, _, _, _, f, _, _, _) -> f) xs,
        fmap (\(_, _, _, _, _, _, g, _, _) -> g) xs,
        fmap (\(_, _, _, _, _, _, _, h, _) -> h) xs,
        fmap (\(_, _, _, _, _, _, _, _, i) -> i) xs
      )
