{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Servant.Client
import Test.QuickCheck
import Test.Syd
import Test.Syd.Servant
import Web

main :: IO ()
main = sydTest $ do
  servantSpec apiProxy endpoints $ do
    let api = client apiProxy

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
