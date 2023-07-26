{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DB (initConnectionPool, initDB)
import Database.Postgres.Temp (toConnectionString)
import Database.Postgres.Temp qualified as PgTemp
import Network.HTTP.Client qualified as HTTP
import Servant.Client
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.Syd
import Test.Syd.Servant
import User
import Web

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

dbCacheSetupFunc :: SetupFunc PgTemp.Cache
dbCacheSetupFunc = SetupFunc $ \test -> PgTemp.withDbCache test

serverSetupFunc :: SetupFunc (ApiRoutes (AsServerT Handler))
serverSetupFunc = do
  dbCache <- dbCacheSetupFunc
  SetupFunc $ \test -> do
    eitherErrOrA <-
      PgTemp.withConfig (PgTemp.cacheConfig dbCache) $ \db -> do
        let connStr = toConnectionString db
        initDB connStr
        conns <- initConnectionPool connStr
        test (routes conns)
    case eitherErrOrA of
      Left err -> expectationFailure $ show err
      Right a -> pure a

serverTest :: TestDef '[HTTP.Manager] ClientEnv -> Spec
serverTest =
  servantSpecWithSetupFunc apiProxy serverSetupFunc . modifyMaxSuccess (`div` 10)

main :: IO ()
main = sydTest $ do
  serverTest $ do
    it "returns pong" $ \clientEnv -> do
      answer <- testClient clientEnv (api.ping)
      answer `shouldBe` "pong"

    it "returns ping" $ \clientEnv -> do
      answer <- testClient clientEnv (api // pong)
      answer `shouldBe` "ping"

    it "says hello" $ \clientEnv ->
      forAll (arbitrary `suchThat` (/= "")) $ \name -> do
        answer <- testClient clientEnv (api // iAm /: name)
        answer `shouldBe` ["Hello " <> name]

  userSpec

userSpec :: Spec
userSpec =
  serverTest $ do
    it "returns the requested users" $ \clientEnv -> do
      let requestedUsers = UserID <$> ["a", "b", "c"]
      answer <- testClient clientEnv (api.userRoutes.getUsers requestedUsers)
      length answer `shouldBe` length requestedUsers
