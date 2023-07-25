{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DB (initConnectionPool, initDB)
import Database.Postgres.Temp (toConnectionString)
import Database.Postgres.Temp qualified as PgTemp
import Servant.Client
import Servant.Server
import Servant.Server.Generic (AsServerT)
import Test.QuickCheck
import Test.Syd
import Test.Syd.Servant
import User
import Web

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

serverSetupFunc :: SetupFunc (ApiRoutes (AsServerT Handler))
serverSetupFunc = do
  eitherErrOrConns <- liftIO $ PgTemp.with $ \db -> do
    let connStr = toConnectionString db
    conns <- initConnectionPool connStr
    initDB connStr
    pure conns
  case eitherErrOrConns of
    Left err -> error $ show err
    Right conns -> pure $ routes conns

main :: IO ()
main = sydTest $ do
  servantSpecWithSetupFunc apiProxy serverSetupFunc $ do
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

userSpec :: Spec
userSpec =
  servantSpecWithSetupFunc apiProxy serverSetupFunc $ do
    it "returns the requested users" $ \clientEnv -> do
      let requestedUsers = UserID <$> ["a", "b", "c"]
      answer <- testClient clientEnv (api.userRoutes.getUsers requestedUsers)
      length answer `shouldBe` length requestedUsers
