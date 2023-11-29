{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Servant.Client ((//), (/:))
import Test.QuickCheck (Arbitrary (arbitrary), forAll, suchThat)
import Test.QuickCheck.Instances.Text ()
import Test.Syd (Spec, it, shouldBe, shouldSatisfy, sydTest)
import TestUtil
import User (UserID (..))
import User.Fake (ensureSomeUsersInDB)
import User.Web
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
userSpec = do
  serverAndClientTest $ do
    it "returns the requested users" $ \(server, clientEnv) -> do
      runOnServer server $ ensureSomeUsersInDB 10
      let requestedUsers = UserID <$> [1, 2, 3]
      answer <- runOnClient clientEnv (api.userRoutes.getOthers requestedUsers)
      length answer `shouldBe` length requestedUsers

    it "can search for users" $ \(server, clientEnv) -> do
      -- The users that are randomly generated have very broad search criteria,
      -- so they should be able to find each other
      runOnServer server $ ensureSomeUsersInDB 10
      answer <- runOnClient clientEnv (api.userRoutes.findLoveForMe)
      length answer `shouldSatisfy` (> 0)
