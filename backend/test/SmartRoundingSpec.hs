module SmartRoundingSpec (spec) where

import Control.Monad (forM_)
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import TestUtil
import User (smartRoundDistanceKm)

spec :: Spec
spec = testThoroughly $ describe "smart distance" $ do
  it "never shows closer than 100 meters" $ property $ \(real, search) ->
    smartRoundDistanceKm real search `shouldSatisfy` (>= 100)

  it "never shows closer than 1000 meters if my search is larger" $ property $ \real ->
    smartRoundDistanceKm real 1.2 `shouldSatisfy` (>= 1000)

  it "large distances are rounded to nearest km" $ property $ \(real, search) ->
    smartRoundDistanceKm (4 + abs real) search `shouldSatisfy` (\res -> res `mod` 1000 == 0)

  describe "examples" $ do
    let exampleTest (realDistance, searchDistance, result) =
          it (unwords ["rounds ", show realDistance, " to ", show result, " with search distance ", show searchDistance]) $
            smartRoundDistanceKm realDistance searchDistance `shouldBe` result

    let examples =
          -- real distance (km), search distance (km), result in meters
          [ (0, 0, 100),
            (0.1, 1, 100),
            (0.3, 0.9, 300),
            (0.6, 0.9, 600),
            (0.8, 0.9, 800),
            (0.1, 1.2, 1000),
            (1.1, 3, 1000),
            (1.3, 3, 1500),
            (1.5, 3, 1500),
            (1.6, 3, 1500),
            (1.8, 3, 2000),
            (2.2, 3, 2000),
            (2.4, 3, 2500)
          ]

    forM_ examples exampleTest
