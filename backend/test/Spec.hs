{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DB (initConnectionPool, initDB)
import Data.Text qualified as T
import Database.Postgres.Temp (toConnectionString)
import Database.Postgres.Temp qualified as PgTemp
import Network.HTTP.Client qualified as HTTP
import Servant (Handler, HasServer (ServerT), serve)
import Servant.Client
import Servant.Server.Generic (AsServerT)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.Syd
import Test.Syd.Wai.Def (applicationSetupFunc, managerSpec)
import User
import Web

instance (HContains l a, HContains l b) => HContains l (a, b) where
  getElem l = (getElem l, getElem l)

instance (HContains l a, HContains l b) => HContains l (HList '[a, b]) where
  getElem l = HCons (getElem l) $ HCons (getElem l) $ HNil

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

dbCacheSetupFunc :: SetupFunc PgTemp.Cache
dbCacheSetupFunc = SetupFunc PgTemp.withDbCache

serverSetupFunc :: PgTemp.Cache -> SetupFunc (ApiRoutes (AsServerT Handler))
serverSetupFunc dbCache = do
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

serverTest :: TestDef '[PgTemp.Cache, HTTP.Manager] ClientEnv -> Spec
serverTest =
  let setupClient :: (PgTemp.Cache, HTTP.Manager) -> SetupFunc ClientEnv
      setupClient (dbCache, man) = serverSetupFunc dbCache >>= clientEnvSetupFunc man

      setupClientHList :: HList '[PgTemp.Cache, HTTP.Manager] -> SetupFunc ClientEnv
      setupClientHList outers = serverSetupFunc (getElem outers) >>= clientEnvSetupFunc (getElem outers)
   in managerSpec
        . setupAroundAll dbCacheSetupFunc
        . setupAroundWith' (\outers () -> setupClientHList outers)

clientEnvSetupFunc :: HTTP.Manager -> ServerT Web.Api Handler -> SetupFunc ClientEnv
clientEnvSetupFunc man server = do
  let application = serve apiProxy server
  p <- applicationSetupFunc application
  pure $
    mkClientEnv
      man
      ( BaseUrl
          Http
          "127.0.0.1"
          (fromIntegral p) -- Safe because it is PortNumber -> Int
          ""
      )

testClientOrError :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClientOrError = flip runClientM

testClient :: ClientEnv -> ClientM a -> IO a
testClient cEnv func = do
  errOrRes <- testClientOrError cEnv func
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right r -> pure r

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
      forAll (arbitrary `suchThat` (\t -> t /= "" && not (T.elem '\NUL' t))) $ \name -> do
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
