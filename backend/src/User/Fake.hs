{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module User.Fake where

import Control.Monad.IO.Class (liftIO)
import DB (runHasql)
import Data.List (genericTake)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Faker (Fake, generateNonDeterministic)
import Faker.Book.Lovecraft qualified
import Faker.Combinators (fakeBoundedEnum, fromRange, listOf, oneof)
import Faker.Creature.Cat qualified
import Faker.DateTime qualified
import Faker.Food qualified
import Faker.FunnyName qualified
import Faker.Name qualified
import Faker.Superhero qualified
import Hasql.TH (singletonStatement)
import Hasql.Transaction (statement)
import SafeMaths (int32ToInt)
import ServerM (ServerM)
import User (NewUserInfo (..), ProfileSection (..), UserID)
import User.Queries (createNewUser)

maybeF :: Fake a -> Fake (Maybe a)
maybeF f = oneof [pure Nothing, Just <$> f]

fakeImgUrl :: Fake Text
fakeImgUrl = do
  imgWidth <- fromRange (200, 600 :: Int)
  imgHeight <- fromRange (200, 600 :: Int)
  pure $ "https://loremflickr.com/" <> T.pack (show imgWidth) <> "/" <> T.pack (show imgHeight) <> "/cat"

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

fakeNewUser :: Fake NewUserInfo
fakeNewUser = do
  newUserName <- do
    T.unwords
      . catMaybes
      <$> sequence
        [ maybeF Faker.Name.prefix,
          Just <$> Faker.Creature.Cat.name,
          Just <$> Faker.FunnyName.name
        ]

  newUserHeaderImageUrl <- fakeImgUrl

  descriptionLength <- fromRange (4, 15 :: Int)
  newUserDescription <- T.unwords <$> listOf descriptionLength Faker.Book.Lovecraft.words

  -- Place users close to each other so they can find each other
  -- Belgium: lat 50.503887 and long 4.469936
  newUserLatitude <- Faker.Combinators.fromRange (49.0, 51.0)
  newUserLongitude <- Faker.Combinators.fromRange (3.0, 5.0)

  newUserBirthday <- Faker.DateTime.dayBetweenYears 1950 2000

  newUserGenderIdentity <- fakeBoundedEnum

  let newUserSearchMinAge = 18
  let newUserSearchMaxAge = 99
  let newUserSearchDistanceM = 1_000_000
  let newUserSearchGenderIdentities = [minBound .. maxBound]

  pure NewUserInfo {..}

ensureSomeUsersInDB :: Int -> ServerM [UserID]
ensureSomeUsersInDB wanted = do
  -- We generate definitely enough users, but might not insert all
  -- We generate these here first, so we can do the check and insert queries as a single transaction
  fakeUsers <- liftIO $ generateNonDeterministic $ listOf wanted fakeNewUser
  runHasql $ do
    currentCount <- statement () [singletonStatement| SELECT COUNT(*) :: int FROM users |]
    let usersToAdd = genericTake (wanted - int32ToInt currentCount) fakeUsers
    mapM createNewUser usersToAdd
