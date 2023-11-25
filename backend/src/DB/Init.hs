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
import User (genderIdentityToSQL, relationshipStatusToSQL)

-- | Initialise the database until we have a proper way of doing migrations
initDB :: Hasql.Pool.Pool -> IO ()
initDB pool =
  runSessionWith pool $
    transaction ReadCommitted Write $
      sql $
        createDBEnum "gender_identity" genderIdentityToSQL
          <> createDBEnum "relationship_status" relationshipStatusToSQL
          <> [uncheckedSql|
                DROP TABLE IF EXISTS greeted_people;
                CREATE TABLE greeted_people (name text NOT NULL);
                DROP TABLE IF EXISTS users;
                CREATE TABLE users (
                  id SERIAL PRIMARY KEY,
                  name text NOT NULL,
                  header_image_url text NOT NULL,
                  last_location_lat float4 NOT NULL,
                  last_location_long float4 NOT NULL,
                  join_day date NOT NULL,
                  birthday date NOT NULL,
                  gender_identity gender_identity NOT NULL,
                  description text NOT NULL,
                  relationship_status relationship_status NOT NULL
                );
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
