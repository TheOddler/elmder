{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module TestUtil where

import App (mkServer)
import Database.Postgres.Temp (toConnectionString)
import Database.Postgres.Temp qualified as PgTemp
import Network.HTTP.Client qualified as HTTP
import Servant (Server, serve)
import Servant.Client
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
import Test.Syd.Wai.Def (applicationSetupFunc, managerSpec)
import Web

api :: ApiRoutes (AsClientT ClientM)
api = client apiProxy

clientTest :: TestDef '[PgTemp.Cache, HTTP.Manager] ClientEnv -> Spec
clientTest =
  let dbCacheSetupFunc :: SetupFunc PgTemp.Cache
      dbCacheSetupFunc = SetupFunc PgTemp.withDbCache

      serverSetupFunc :: PgTemp.Cache -> SetupFunc (Server Api)
      serverSetupFunc dbCache = SetupFunc $ \test -> do
        eitherErrOrA <- PgTemp.withConfig (PgTemp.cacheConfig dbCache) $ \db -> do
          server <- mkServer $ toConnectionString db
          test server
        case eitherErrOrA of
          Left err -> expectationFailure $ show err
          Right a -> pure a

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

      setupClient :: HList '[PgTemp.Cache, HTTP.Manager] -> SetupFunc ClientEnv
      setupClient (HCons dbCache (HCons man HNil)) = do
        server <- serverSetupFunc dbCache
        clientSetupFunc man server
   in managerSpec
        . setupAroundAll dbCacheSetupFunc
        . setupAroundWithAll (\outers () -> setupClient outers)
        . modifyMaxSuccess (`div` 10)
