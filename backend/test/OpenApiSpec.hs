{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenApiSpec (spec) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Servant.OpenApi (toOpenApi)
import Test.Syd
import Web

spec :: Spec
spec =
  it "spits out an OpenAPI schema" $
    pureGoldenLazyByteStringFile "openapi/openapi.json" $
      encodePretty $
        toOpenApi apiProxy
