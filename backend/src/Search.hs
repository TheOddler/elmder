{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Search where

import Control.Monad.IO.Class (liftIO)
import DB (runHasql)
import Data.Int (Int32)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Data.Vector (fromList, toList)
import GHC.Generics (Generic)
import Hasql.TH (vectorStatement)
import Hasql.Transaction (statement)
import Servant (JSON, Post, ReqBody, (:-), (:>))
import Servant.Elm qualified
import Servant.Server.Generic (AsServerT)
import ServerM (ServerM)
import User (GenderIdentity, Location (..), UserID (..), genderIdentityToSQL)

data SearchParameters = SearchParameters
  { maxResults :: Int32,
    ageMin :: Int,
    ageMax :: Int,
    location :: Location,
    distanceKm :: Float,
    genderIdentity :: [GenderIdentity]
  }
  deriving (Generic)

Servant.Elm.deriveBoth Servant.Elm.defaultOptions ''SearchParameters

newtype SearchRoutes mode = SearchRoutes
  { searchUsers :: mode :- "ids" :> ReqBody '[JSON] SearchParameters :> Post '[JSON] [UserID]
  }
  deriving (Generic)

searchRoutes :: SearchRoutes (AsServerT ServerM)
searchRoutes = SearchRoutes {..}
  where
    searchUsers :: SearchParameters -> ServerM [UserID]
    searchUsers SearchParameters {..} = do
      currentTime <- liftIO getCurrentTime
      let (currentYear, currentDayOfYear) = toOrdinalDate currentTime.utctDay
      let minDate = fromOrdinalDate (currentYear - toInteger ageMax) currentDayOfYear
      let maxDate = fromOrdinalDate (currentYear - toInteger ageMin) currentDayOfYear
      let oneDegLatitudeInKm = 110.574
      let oneDegLongitudeInKm = 111.320 * cos (location.locationLatitude * pi / 180)
      let minLat = location.locationLatitude - distanceKm / oneDegLatitudeInKm
      let maxLat = location.locationLatitude + distanceKm / oneDegLatitudeInKm
      let minLong = location.locationLongitude - distanceKm / oneDegLongitudeInKm
      let maxLong = location.locationLongitude + distanceKm / oneDegLongitudeInKm
      ids <-
        runHasql $
          statement
            ( maxResults,
              minDate,
              maxDate,
              minLat,
              maxLat,
              minLong,
              maxLong,
              fromList $ genderIdentityToSQL <$> genderIdentity
            )
            [vectorStatement|
              SELECT id :: int
              FROM users
              WHERE birthday BETWEEN $2 :: date AND $3 :: date
                AND last_location_lat BETWEEN $4 :: float4 AND $5 :: float4
                AND last_location_long BETWEEN $6 :: float4 AND $7 :: float4
                AND gender_identity = ANY($8 :: text[] :: gender_identity[])
              LIMIT $1 :: int
            |]
      pure $ UserID <$> toList ids
