{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Servant.Client ((//), (/:))
import Test.QuickCheck (Arbitrary (arbitrary), forAll, suchThat)
import Test.QuickCheck.Instances.Text ()
import Test.Syd
  ( Spec,
    it,
    shouldBe,
    sydTest,
  )
import Test.Syd.Servant (testClient)
import TestUtil
import User (UserID (UserID), UserRoutes (getUsers))
import Web

main :: IO ()
main = sydTest $ do
  clientTest $ do
    it "returns pong" $ \clientEnv -> do
      answer <- testClient clientEnv (api.ping)
      answer `shouldBe` "pong"

    it "returns ping" $ \clientEnv -> do
      answer <- testClient clientEnv (api // pong)
      answer `shouldBe` "ping"

    it "says hello" $ \clientEnv ->
      forAll (arbitrary `suchThat` (\t -> t /= "" && not (T.elem '\NUL' t))) $ \name -> do
        answer <- testClient clientEnv (api // iAm /: name)
        answer `shouldBe` ["Hello " <> name]

  userSpec

userSpec :: Spec
userSpec =
  clientTest $ do
    it "returns the requested users" $ \clientEnv -> do
      let requestedUsers = UserID <$> ["a", "b", "c"]
      answer <- testClient clientEnv (api.userRoutes.getUsers requestedUsers)
      length answer `shouldBe` length requestedUsers
