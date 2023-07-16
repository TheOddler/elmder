{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Servant.Client
import Servant.OpenApi (toOpenApi)
import Test.QuickCheck
import Test.Syd
import Test.Syd.Servant
import User
import Web

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

main :: IO ()
main = sydTest $ do
  servantSpec apiProxy routes $ do
    it "returns pong" $ \clientEnv -> do
      answer <- testClient clientEnv (api.ping)
      answer `shouldBe` "pong"

    it "returns ping" $ \clientEnv -> do
      answer <- testClient clientEnv (api // pong)
      answer `shouldBe` "ping"

    it "says hello" $ \clientEnv ->
      forAll (arbitrary `suchThat` (/= "")) $ \name -> do
        answer <- testClient clientEnv (api // iAm /: name)
        answer `shouldBe` "Hello " <> name

  userSpec
  openApiSpec

userSpec :: Spec
userSpec =
  servantSpec apiProxy routes $ do
    it "returns the requested users" $ \clientEnv -> do
      let requestedUsers = UserID <$> ["a", "b", "c"]
      answer <- testClient clientEnv (api.userRoutes.getUsers requestedUsers)
      length answer `shouldBe` length requestedUsers

openApiSpec :: Spec
openApiSpec =
  it "spits out an OpenAPI schema" $
    pureGoldenLazyByteStringFile "openapi/openapi.json" $
      encodePretty $
        toOpenApi apiProxy
