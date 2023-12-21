{-# LANGUAGE QuasiQuotes #-}

module DB.Init where

import DB (runSessionWith, sqlTransaction_)
import Data.String (fromString)
import Data.Text (Text, intercalate, unpack)
import Hasql.Interpolate (Sql, sql)
import Hasql.Pool (Pool)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (Write), transaction)
import User (relationshipStatusToSQL)
import User.GenderIdentity (genderIdentityToSQL)
import User.Impressions (impressionToSQL)

-- | Initialise the database until we have a proper way of doing migrations
initDB :: Pool -> IO ()
initDB pool =
  runSessionWith pool $
    transaction ReadCommitted Write $
      mapM_ sqlTransaction_ allSql
  where
    allSql :: [Sql]
    allSql =
      [[sql|CREATE EXTENSION IF NOT EXISTS earthdistance CASCADE;|]]
        <> createDBEnum "gender_identity" genderIdentityToSQL
        <> createDBEnum "relationship_status" relationshipStatusToSQL
        <> createDBEnum "impression" impressionToSQL
        <> [ [sql|DROP TABLE IF EXISTS users CASCADE;|],
             [sql|CREATE TABLE users (
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
                    );|],
             [sql|CREATE INDEX ON users (location);|],
             [sql|CREATE INDEX ON users (birthday);|],
             [sql|CREATE INDEX ON users (gender_identity);|],
             [sql|CREATE INDEX ON users (relationship_status);|],
             [sql|DROP TABLE IF EXISTS user_search_gender_identities CASCADE;|],
             [sql|CREATE TABLE user_search_gender_identities (
                      user_id SERIAL NOT NULL REFERENCES users(id),
                      search_gender_identity gender_identity NOT NULL,
                      UNIQUE (user_id, search_gender_identity)
                    );|],
             [sql|CREATE INDEX ON user_search_gender_identities (user_id);|],
             [sql|DROP TABLE IF EXISTS impressions CASCADE;|],
             [sql|CREATE TABLE impressions (
                      user_id SERIAL NOT NULL REFERENCES users(id),
                      impression impression NOT NULL,
                      other_user_id SERIAL NOT NULL REFERENCES users(id),
                      timestamp TIMESTAMPTZ NOT NULL,
                      UNIQUE (user_id, other_user_id)
                    );|],
             [sql|CREATE INDEX ON impressions (user_id);|]
           ]

    createDBEnum :: (Bounded a, Enum a) => String -> (a -> Text) -> [Sql]
    createDBEnum enumName toSqlName =
      [ [sql|DROP TYPE IF EXISTS ^{fromString enumName} CASCADE;|],
        [sql|CREATE TYPE ^{fromString enumName} AS ENUM (^{enumValuesSql toSqlName})|]
      ]

    enumValuesSql :: (Bounded a, Enum a) => (a -> Text) -> Sql
    enumValuesSql toSqlName =
      fromString $
        unpack $
          intercalate
            ", "
            (wrapInSingleQuotes . toSqlName <$> [minBound .. maxBound])

    wrapInSingleQuotes t = "'" <> t <> "'"
