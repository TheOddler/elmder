module PingPongSpec (spec) where

import Servant.Client ((//))
import Test.Syd (Spec, it, shouldBe)
import TestUtil
import Web

spec :: Spec
spec = do
  clientTest $ do
    it "returns pong" $ \clientEnv -> do
      answer <- runOnClient clientEnv (api.ping)
      answer `shouldBe` "pong"

    it "returns ping" $ \clientEnv -> do
      answer <- runOnClient clientEnv (api // pong)
      answer `shouldBe` "ping"
