{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Servant.Client ((//), (/:))
import Test.QuickCheck (Arbitrary (arbitrary), forAll, suchThat)
import Test.QuickCheck.Instances.Text ()
import Test.Syd (Spec, it, shouldBe, shouldSatisfy, sydTest, xit)
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
    xit "returns the requested user's info" $ \(server, clientEnv) -> do
      runOnServer server $ ensureSomeUsersInDB 10
      _ <- runOnClient clientEnv (api.userRoutes.getUserExtendedInfo $ UserID 1)
      pure ()

    it "can search for users" $ \(server, clientEnv) -> do
      -- The users that are randomly generated have very broad search criteria,
      -- so they should be able to find each other
      runOnServer server $ ensureSomeUsersInDB 10
      answer <- runOnClient clientEnv (api.userRoutes.getSearch)
      length answer `shouldSatisfy` (> 0)
