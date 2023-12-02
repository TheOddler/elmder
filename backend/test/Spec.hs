{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Servant.Client ((//))
import Test.Syd (Spec, it, shouldBe, shouldSatisfy, sydTest)
import TestUtil
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

  userSpec

userSpec :: Spec
userSpec = do
  serverAndClientTest $ do
    it "returns the requested user's info" $ \(server, clientEnv) -> do
      ids <- runOnServer server $ ensureSomeUsersInDB 10
      mapM_ (runOnClient clientEnv . api.userRoutes.getUserExtendedInfo) ids

    it "can search for users" $ \(server, clientEnv) -> do
      -- The users that are randomly generated have very broad search criteria,
      -- so they should be able to find each other
      _ <- runOnServer server $ ensureSomeUsersInDB 10
      answer <- runOnClient clientEnv (api.userRoutes.getSearch)
      length answer `shouldSatisfy` (> 0)
