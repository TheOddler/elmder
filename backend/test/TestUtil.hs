{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module TestUtil where

import Database.Postgres.Temp (toConnectionString)
import Database.Postgres.Temp qualified as PgTemp
import Network.HTTP.Client qualified as HTTP
import Servant (Server, ServerError, runHandler, serve)
import Servant.Client
  ( AsClientT,
    BaseUrl (BaseUrl),
    ClientEnv,
    ClientError,
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
  )
import Server (mkServer, mkServerEnv)
import ServerM (ServerEnv, ServerM, toServantHandler)
import Test.QuickCheck.Instances.Text ()
import Test.Syd
  ( HList (..),
    SetupFunc (SetupFunc),
    Spec,
    TestDef,
    expectationFailure,
    modifyMaxSuccess,
    setupAroundAll,
    setupAroundWith,
    setupAroundWithAll,
  )
import Test.Syd.Servant (testClientOrError)
import Test.Syd.Wai.Def (applicationSetupFunc, managerSpec)
import Web (Api, ApiRoutes, apiProxy)

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

serverAndClientTest :: TestDef '[PgTemp.Cache, HTTP.Manager] (ServerEnv, ClientEnv) -> Spec
serverAndClientTest =
  let dbCacheSetupFunc :: SetupFunc PgTemp.Cache
      dbCacheSetupFunc =
        let config =
              PgTemp.defaultCacheConfig
                { -- We overwrite the cacheDirectory here as the default uses the home folder which is disallowed during nix builds, so that would make the tests fail when using nix to build or test.
                  PgTemp.cacheDirectoryType = PgTemp.Permanent "/tmp/.tmp-postgres"
                }
         in SetupFunc $ PgTemp.withDbCacheConfig config

      serverEnvSetupFunc :: PgTemp.Cache -> SetupFunc ServerEnv
      serverEnvSetupFunc dbCache = SetupFunc $ \test -> do
        errOrA <- PgTemp.withConfig (PgTemp.cacheConfig dbCache) $ \db -> do
          serverEnv <- mkServerEnv $ toConnectionString db
          test serverEnv
        case errOrA of
          Left err -> expectationFailure $ show err
          Right a -> pure a

      serverSetupFunc :: ServerEnv -> SetupFunc (Server Api)
      serverSetupFunc serverEnv = SetupFunc $ \test -> do
        server <- mkServer serverEnv
        test server

      clientSetupFunc :: HTTP.Manager -> Server Api -> SetupFunc ClientEnv
      clientSetupFunc man server = do
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

      setupClient :: HList '[PgTemp.Cache, HTTP.Manager] -> SetupFunc (ServerEnv, ClientEnv)
      setupClient (HCons dbCache (HCons man HNil)) = do
        serverEnv <- serverEnvSetupFunc dbCache
        server <- serverSetupFunc serverEnv
        clientEnv <- clientSetupFunc man server
        pure (serverEnv, clientEnv)
   in managerSpec
        . setupAroundAll dbCacheSetupFunc
        . setupAroundWithAll (\outers () -> setupClient outers)
        . modifyMaxSuccess (`div` 10)

serverTest :: TestDef '[PgTemp.Cache, HTTP.Manager] ServerEnv -> Spec
serverTest testDef =
  serverAndClientTest $ setupAroundWith (pure . fst) testDef

clientTest :: TestDef '[PgTemp.Cache, HTTP.Manager] ClientEnv -> Spec
clientTest testDef =
  serverAndClientTest $ setupAroundWith (pure . snd) testDef

runOnServerOrError :: ServerEnv -> ServerM a -> IO (Either ServerError a)
runOnServerOrError serverEnv = runHandler . toServantHandler serverEnv

runOnServer :: ServerEnv -> ServerM a -> IO a
runOnServer serverEnv serverM = do
  errOrA <- runOnServerOrError serverEnv serverM
  case errOrA of
    Left err -> expectationFailure $ show err
    Right a -> pure a

runOnClientOrError :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runOnClientOrError = testClientOrError

runOnClient :: ClientEnv -> ClientM a -> IO a
runOnClient clientEnv clientM = do
  errOrA <- runOnClientOrError clientEnv clientM
  case errOrA of
    Left err -> expectationFailure $ show err
    Right a -> pure a

testThoroughly :: Spec -> Spec
testThoroughly =
  modifyMaxSuccess (* 10)
