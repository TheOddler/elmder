{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module TestUtil where

import DB (initConnectionPool, initDB)
import Database.Postgres.Temp (toConnectionString)
import Database.Postgres.Temp qualified as PgTemp
import Network.HTTP.Client qualified as HTTP
import Servant (Handler)
import Servant.Client
import Servant.Server.Generic (AsServerT)
import SydTestExtra (setupAroundWithAll)
import Test.QuickCheck.Instances.Text ()
import Test.Syd
  ( HList (..),
    SetupFunc (SetupFunc),
    Spec,
    TestDef,
    expectationFailure,
    modifyMaxSuccess,
    setupAroundAll,
  )
import Test.Syd.Servant (clientEnvSetupFunc)
import Test.Syd.Wai.Def (managerSpec)
import Web

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

clientTest :: TestDef '[PgTemp.Cache, HTTP.Manager] ClientEnv -> Spec
clientTest =
  let dbCacheSetupFunc :: SetupFunc PgTemp.Cache
      dbCacheSetupFunc = SetupFunc PgTemp.withDbCache

      serverSetupFunc :: PgTemp.Cache -> SetupFunc (ApiRoutes (AsServerT Handler))
      serverSetupFunc dbCache = SetupFunc $ \test -> do
        eitherErrOrA <- PgTemp.withConfig (PgTemp.cacheConfig dbCache) $ \db -> do
          let connStr = toConnectionString db
          initDB connStr
          conns <- initConnectionPool connStr
          test (routes conns)
        case eitherErrOrA of
          Left err -> expectationFailure $ show err
          Right a -> pure a

      setupClient :: HList '[PgTemp.Cache, HTTP.Manager] -> SetupFunc ClientEnv
      setupClient (HCons dbCache (HCons man HNil)) = do
        server <- serverSetupFunc dbCache
        clientEnvSetupFunc apiProxy man server
   in managerSpec
        . setupAroundAll dbCacheSetupFunc
        . setupAroundWithAll (\outers () -> setupClient outers)
        . modifyMaxSuccess (`div` 10)
