{-# LANGUAGE TemplateHaskell #-}

module User.Impressions where

import Data.Text (Text)
import Elm.Derive (deriveBoth)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Elm qualified
import Util (reverseEnumToText)

-- | Note that the below descriptions are just ideas, not all of it is implemented yet
data Impression
  = -- | The user liked the other user, if they like each other it's a match
    ImpressionLike
  | -- | The user disliked the other user, we will not show them to each other again
    ImpressionDislike
  | -- | The user is undecided, we won't show the user to them any more, but the other user can still see them
    ImpressionDecideLater
  | -- | The user REALLY likes the other user, the other user will get a notification about this
    ImpressionSuperLike
  deriving (Eq, Show, Enum, Bounded)

deriveBoth Servant.Elm.defaultOptions ''Impression

instance ToHttpApiData Impression where
  toUrlPiece = impressionToSQL

instance FromHttpApiData Impression where
  parseUrlPiece t = case reverseEnumToText toUrlPiece t of
    Nothing -> Left $ "Invalid impression: " <> t
    Just x -> Right x

-- | Convert a 'RelationshipStatus' to a SQL name for it.
impressionToSQL :: Impression -> Text
impressionToSQL = \case
  ImpressionLike -> "like"
  ImpressionDislike -> "dislike"
  ImpressionDecideLater -> "decide_later"
  ImpressionSuperLike -> "super_like"

impressionFromSQL :: Text -> Maybe Impression
impressionFromSQL = reverseEnumToText impressionToSQL
