{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module User where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic (AsServerT)

newtype UserID = UserID {unUserID :: Text}
  deriving (Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON)

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
  deriving (Generic, ToJSON, FromJSON)

data RelationshipStatus
  = RelationshipStatusSingle
  | RelationshipStatusMarried
  | RelationshipStatusInRelationship
  deriving (Generic, ToJSON, FromJSON)

data UserSection
  = UserSectionGeneric {header :: Text, content :: Text}
  | UserSectionImages {images :: [Text], description :: Text}
  | UserSectionQuestionAndAnswer {question :: Text, answer :: Text}
  deriving (Generic, ToJSON, FromJSON)

data UserRoutes mode = UserRoutes
  { getUsers :: mode :- "many" :> Capture "id" [UserID] :> Get '[JSON] [User],
    getSomeUserIDs :: mode :- "exampleIDs" :> Get '[JSON] [UserID]
  }
  deriving (Generic)

userRoutes :: UserRoutes (AsServerT Handler)
userRoutes = UserRoutes {..}
  where
    getUsers :: [UserID] -> Handler [User]
    getUsers = liftIO . mapM genFakeUser

    genFakeUser :: UserID -> IO User
    genFakeUser userID =
      pure
        User
          { userID = userID,
            userName = unUserID userID,
            userHeaderImage = "https://placekitten.com/200/200",
            userDescription = unUserID userID,
            relationshipStatus = RelationshipStatusSingle,
            userSections = []
          }

    getSomeUserIDs :: Handler [UserID]
    getSomeUserIDs = pure $ UserID . T.pack . show <$> [1 .. 10 :: Int]
