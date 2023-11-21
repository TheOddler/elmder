{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module User where

import DB (runHasql)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (fromList)
import Elm.Derive (deriveBoth)
import Faker (generateNonDeterministic)
import GHC.Generics (Generic)
import Hasql.TH (vectorStatement)
import Servant.Elm qualified
import ServerM (ServerM)

newtype UserID = UserID {unUserID :: Int32}
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
          description :: text
        FROM users
        WHERE id = ANY ($1 :: int[])
      |]
  pure $ toUser <$> toList rows
  where
    toUser (userId, name, img, descr) =
      User
        { userID = UserID userId,
          userName = name,
          userHeaderImage = img,
          userDescription = descr,
          userRelationshipStatus = RelationshipStatusSingle,
          userSections = []
        }
