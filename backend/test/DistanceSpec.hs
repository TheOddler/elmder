{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DistanceSpec (spec) where

import Control.Monad (forM_)
import DB (runHasql)
import Data.Time (fromGregorian)
import SafeMath (int32ToInt)
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import TestUtil
import User (NewUserInfo (..), UserOverviewInfo (..), createNewUser, searchFor, smartRoundDistanceM)
import User.GenderIdentity (GenderIdentity (..))

spec :: Spec
spec = testThoroughly $ describe "smart distance rounding" $ do
  it "never shows closer than 100 meters" $ property $ \(real, search) ->
    smartRoundDistanceM real search `shouldSatisfy` (>= 100)

  it "never shows closer than 1000 meters if my search is larger" $ property $ \real ->
    smartRoundDistanceM real 1200 `shouldSatisfy` (>= 1000)

  it "large distances are rounded to nearest km" $ property $ \(real, search) -> do
    let largeDistance = 4000 + abs real
    let smartDistance = smartRoundDistanceM largeDistance search
    smartDistance `shouldSatisfy` (\res -> res `mod` 1000 == 0)
    smartDistance `shouldBe` round (largeDistance / 1000) * 1000

  describe "examples" $ do
    let exampleTest (realDistance, searchDistance, result) =
          it (unwords ["rounds ", show realDistance, " to ", show result, " with search distance ", show searchDistance]) $
            smartRoundDistanceM realDistance searchDistance `shouldBe` result

    let examples =
          -- real distance (km), search distance (km), result in meters
          [ (0, 0, 100),
            (100, 100, 100),
            (300, 900, 300),
            (600, 900, 600),
            (800, 900, 800),
            (100, 1200, 1000),
            (1100, 3000, 1000),
            (1300, 3000, 1500),
            (1500, 3000, 1500),
            (1600, 3000, 1500),
            (1800, 3000, 2000),
            (2200, 3000, 2000),
            (2400, 3000, 2500)
          ]

    forM_ examples exampleTest
