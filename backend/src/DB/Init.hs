{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Init where

import DB (runSessionWith)
import Data.ByteString (ByteString, intercalate)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Hasql.Pool qualified
import Hasql.TH (uncheckedSql)
import Hasql.Transaction (sql)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (Write), transaction)
import User (relationshipStatusToSQL)
import User.GenderIdentity (genderIdentityToSQL)
import User.Impressions (impressionToSQL)

-- | Initialise the database until we have a proper way of doing migrations
initDB :: Hasql.Pool.Pool -> IO ()
initDB pool =
  runSessionWith pool $
    transaction ReadCommitted Write $
      sql $
        "CREATE EXTENSION IF NOT EXISTS earthdistance CASCADE;\n\n"
          <> createDBEnum "gender_identity" genderIdentityToSQL
          <> createDBEnum "relationship_status" relationshipStatusToSQL
          <> createDBEnum "impression" impressionToSQL
          <> [uncheckedSql|
                DROP TABLE IF EXISTS users CASCADE;
                CREATE TABLE users (
                  id SERIAL PRIMARY KEY,
                  name text NOT NULL,
                  header_image_url text NOT NULL,
                  location earth NOT NULL,
                  join_day date NOT NULL,
                  birthday date NOT NULL,
                  gender_identity gender_identity NOT NULL,
                  description text NOT NULL,
                  relationship_status relationship_status NOT NULL,
                  search_age_min int2 NOT NULL,
                  search_age_max int2 NOT NULL,
                  search_distance_km int2 NOT NULL
                );
                CREATE INDEX ON users (location);
                CREATE INDEX ON users (birthday);
                CREATE INDEX ON users (gender_identity);
                CREATE INDEX ON users (relationship_status);
                
                DROP TABLE IF EXISTS user_search_gender_identities CASCADE;
                CREATE TABLE user_search_gender_identities (
                  user_id SERIAL NOT NULL REFERENCES users(id),
                  search_gender_identity gender_identity NOT NULL,
                  UNIQUE (user_id, search_gender_identity)
                );
                CREATE INDEX ON user_search_gender_identities (user_id);

                DROP TABLE IF EXISTS impressions CASCADE;
                CREATE TABLE impressions (
                  user_id SERIAL NOT NULL REFERENCES users(id),
                  impression impression NOT NULL,
                  other_user_id SERIAL NOT NULL REFERENCES users(id),
                  timestamp TIMESTAMPTZ NOT NULL,
                  UNIQUE (user_id, other_user_id)
                );
                CREATE INDEX ON impressions (user_id);
            |]
  where
    createDBEnum :: (Bounded a, Enum a) => ByteString -> (a -> Text) -> ByteString
    createDBEnum enumName toSqlName =
      "DROP TYPE IF EXISTS "
        <> enumName
        <> " CASCADE;\n"
        <> "CREATE TYPE "
        <> enumName
        <> " AS ENUM ("
        <> intercalate ", " (encodeUtf8 . wrapInSingleQuotes . toSqlName <$> [minBound .. maxBound])
        <> ");\n"

    wrapInSingleQuotes t = "'" <> t <> "'"
