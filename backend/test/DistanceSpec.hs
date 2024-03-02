module DistanceSpec (spec) where

import Control.Monad (forM_)
import DB (runHasql)
import Data.Time (fromGregorian)
import SafeMaths (int32ToInt)
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import TestUtil
import User (NewUserInfo (..), UserOverviewInfo (..), smartRoundDistanceM)
import User.GenderIdentity (GenderIdentity (..))
import User.Queries (createNewUser, searchFor)

spec :: Spec
spec = do
  describe "user distance examples" $ serverTest $ do
    let examples =
          [ ("0 to 1", 0, 0, 1, 1, 157_000),
            ("20 to 21", 20, 20, 21, 21, 153_000),
            ("Pole to pole", 90, 0, -90, 0, 20_038_000), -- Actually distance is 20,003.8 km, but close enough
            ("Antwerp to Brussels", 51.217778, 4.400278, 50.846667, 4.3525, 41_000),
            ("Antwerp to Amsterdam", 51.217778, 4.400278, 52.374444, 4.897222, 133_000),
            ("Antwerp to Westmalle", 51.217778, 4.400278, 51.283333, 4.683333, 21_000),
            ("Antwerp to Borgerhout", 51.217778, 4.400278, 51.2117, 4.4407, 3_000),
            ("Within Antwerp", 51.217778, 4.400278, 51.217799, 4.400299, 1_000)
          ]
    forM_ examples $ \(description, lat1, long1, lat2, long2, result) ->
      it description $ \server -> do
        let user1Info =
              NewUserInfo
                { newUserName = "First user",
                  newUserDescription = "Description",
                  newUserHeaderImageUrl = "nothing.jpg",
                  newUserLatitude = lat1,
                  newUserLongitude = long1,
                  newUserBirthday = fromGregorian 2001 01 01,
                  newUserGenderIdentity = Other,
                  newUserSearchMinAge = 18,
                  newUserSearchMaxAge = 99,
                  newUserSearchDistanceM = result + 1000,
                  newUserSearchGenderIdentities = [Other]
                }
        let user2Info =
              NewUserInfo
                { newUserName = "Second user",
                  newUserDescription = "Description 2",
                  newUserHeaderImageUrl = "empty.jpg",
                  newUserLatitude = lat2,
                  newUserLongitude = long2,
                  newUserBirthday = fromGregorian 2002 02 02,
                  newUserGenderIdentity = Other,
                  newUserSearchMinAge = 18,
                  newUserSearchMaxAge = 99,
                  newUserSearchDistanceM = result + 1000,
                  newUserSearchGenderIdentities = [Other]
                }
        id1 <- runOnServer server $ runHasql $ createNewUser user1Info
        id2 <- runOnServer server $ runHasql $ createNewUser user2Info
        foundUsers <- runOnServer server $ runHasql $ searchFor id1 10
        case foundUsers of
          [] -> expectationFailure "No users found, should have been exactly one"
          (_ : _ : _) -> expectationFailure "More than one user found"
          [foundUser] -> do
            foundUser.userId `shouldBe` id2
            foundUser.userDistanceM `shouldBe` int32ToInt result

  testThoroughly $ describe "smart distance rounding" $ do
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
