module UserSpec (spec) where

import Test.Syd (Spec, it, shouldSatisfy)
import TestUtil
import User.Fake (ensureSomeUsersInDB)
import User.Web
import Web

spec :: Spec
spec = do
  serverAndClientTest $ do
    it "returns the requested user's info" $ \(server, clientEnv) -> do
      ids <- runOnServer server $ ensureSomeUsersInDB 10
      mapM_ (runOnClient clientEnv . api.userRoutes.getUserInfo) ids

    it "can search for users" $ \(server, clientEnv) -> do
      -- The users that are randomly generated have very broad search criteria,
      -- so they should be able to find each other
      _ <- runOnServer server $ ensureSomeUsersInDB 10
      answer <- runOnClient clientEnv api.userRoutes.getSearch
      length answer `shouldSatisfy` (> 0)
