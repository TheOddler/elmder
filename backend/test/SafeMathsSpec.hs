module SafeMathsSpec (spec) where

import Data.Time.Calendar (fromGregorian)
import SafeMaths
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import TestUtil

spec :: Spec
spec = testThoroughly $ do
  describe "round to nearest" $ do
    it "never errors" $ property $ \(number, rounding) ->
      roundToNearest rounding number `shouldSatisfy` const True

    describe "examples" $ do
      let exampleTest (number, result100, result500, result1000) =
            it (unwords ["can round ", show number]) $ do
              roundToNearest 100 number `shouldBe` result100
              roundToNearest 500 number `shouldBe` result500
              roundToNearest 1000 number `shouldBe` result1000

      mapM_
        exampleTest
        [ (0, 0, 0, 0),
          (1, 0, 0, 0),
          (10, 0, 0, 0),
          (50, 100, 0, 0),
          (51, 100, 0, 0),
          (99, 100, 0, 0),
          (120, 100, 0, 0),
          (150, 200, 0, 0),
          (220, 200, 0, 0),
          (250, 300, 500, 0),
          (740, 700, 500, 1000),
          (760, 800, 1000, 1000),
          (900, 900, 1000, 1000),
          (999, 1000, 1000, 1000),
          (1149, 1100, 1000, 1000),
          (54321, 54300, 54500, 54000),
          (99999, 100000, 100000, 100000)
        ]

  describe "age calculation" $ do
    describe "examples" $ do
      let exampleTest (today, birthday, result) =
            it (unwords ["can calculate age of", show birthday, "on", show today]) $ do
              age today birthday `shouldBe` result

      mapM_
        exampleTest
        [ (fromGregorian 2020 1 1, fromGregorian 2000 1 1, 20),
          (fromGregorian 2020 1 1, fromGregorian 2000 1 2, 19),
          (fromGregorian 2020 1 1, fromGregorian 2000 12 31, 19),
          (fromGregorian 2020 1 1, fromGregorian 2001 1 1, 19),
          (fromGregorian 2020 1 1, fromGregorian 2001 1 2, 18),
          (fromGregorian 2020 1 1, fromGregorian 2001 12 31, 18)
        ]
