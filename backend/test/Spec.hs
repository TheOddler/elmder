{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Search (SearchParameters (..), SearchRoutes (..))
import Servant.Client ((//), (/:))
import Test.QuickCheck (Arbitrary (arbitrary), forAll, suchThat)
import Test.QuickCheck.Instances.Text ()
import Test.Syd (Spec, it, shouldBe, shouldSatisfy, sydTest)
import TestUtil
import User (Location (..), UserID (..))
import User.Fake (ensureSomeUsersInDB)
import User.Web (UserRoutes (..))
import Web

main :: IO ()
main = sydTest $ do
  clientTest $ do
    it "returns pong" $ \clientEnv -> do
      answer <- runOnClient clientEnv (api.ping)
      answer `shouldBe` "pong"

    it "returns ping" $ \clientEnv -> do
      answer <- runOnClient clientEnv (api // pong)
      answer `shouldBe` "ping"

    let isValidText t = t /= "" && not (T.elem '\NUL' t)

    it "says hello" $ \clientEnv ->
      forAll (arbitrary `suchThat` isValidText) $ \name -> do
        answer <- runOnClient clientEnv (api // iAm /: name)
        answer `shouldBe` ["Hello " <> name]

    it "says hello to everybody" $ \clientEnv ->
      forAll (arbitrary `suchThat` (\(n, ns) -> isValidText n && all isValidText ns)) $ \(name, moreNames) -> do
        forM_ moreNames $ \n -> do
          runOnClient clientEnv (api // iAm /: n)
        answer <- runOnClient clientEnv (api // iAm /: name)
        answer `shouldBe` ("Hello " <>) <$> (moreNames <> [name])

  userSpec

userSpec :: Spec
userSpec =
  serverAndClientTest $ do
    it "returns the requested users" $ \(server, clientEnv) -> do
      runOnServer server $ ensureSomeUsersInDB 10
      let requestedUsers = UserID <$> [1, 2, 3]
      answer <- runOnClient clientEnv (api.userRoutes.getUsers requestedUsers)
      length answer `shouldBe` length requestedUsers

    it "can search" $ \(server, clientEnv) -> do
      -- This test is a bit shitty, but the idea is just to have something that calls this new endpoint, and with the search parameters so wide it'll always return something
      runOnServer server $ ensureSomeUsersInDB 1_000
      let searchParams =
            SearchParameters
              { maxResults = 20,
                ageMin = 0,
                ageMax = 1000,
                location = Location 20 20,
                distanceKm = 100000,
                genderIdentity = [minBound .. maxBound]
              }
      userIDs <- runOnClient clientEnv (api.searchRoutes.searchUsers searchParams)
      length userIDs `shouldSatisfy` (> 0)
