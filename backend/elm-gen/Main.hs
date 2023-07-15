{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace qualified as Debug
import Elm.TyRep
import Servant
import Servant.Elm
import Servant.Foreign (Foreign, HasForeign (..))
import User qualified
import Web qualified

-- Taken from https://hackage.haskell.org/package/servant-foreign-0.16/docs/src/Servant.Foreign.Internal.html#line-518
-- But that is only available on servant-foreign 0.16, which is not in any stackage lts yet
instance HasForeign lang ftype (ToServantApi r) => HasForeign lang ftype (NamedRoutes r) where
  type Foreign ftype (NamedRoutes r) = Foreign ftype (ToServantApi r)

  foreignFor lang ftype Proxy =
    foreignFor lang ftype (Proxy :: Proxy (ToServantApi r))

-- deriveElmDef defaultOptions ''User.UserID
instance IsElmDefinition User.UserID where
  compileElmDef :: Proxy User.UserID -> ETypeDef
  -- This just makes an `type alias UserID  = String`
  -- compileElmDef Proxy = ETypePrimAlias $ EPrimAlias (ETypeName "UserID" []) (ETyCon $ ETCon "String")
  compileElmDef _ = ETypeSum $ ESum (ETypeName "UserID" []) [STC "UserID" "UserID" $ Anonymous [ETyCon $ ETCon "String"]] defSumEncoding False False

deriveElmDef defaultOptions ''User.User
deriveElmDef defaultOptions ''User.RelationshipStatus
deriveElmDef defaultOptions ''User.UserSection

main :: IO ()
main = do
  putStrLn "Generating elm..."
  generateElmModuleWith
    defElmOptions
      { urlPrefix = Dynamic,
        elmToString = myElmToString
      }
    [ "Generated",
      "BackendApi"
    ]
    elmImports
    "frontend/src"
    [ DefineElm (Proxy :: Proxy User.UserID),
      DefineElm (Proxy :: Proxy User.User),
      DefineElm (Proxy :: Proxy User.RelationshipStatus),
      DefineElm (Proxy :: Proxy User.UserSection)
    ]
    (Proxy :: Proxy (ToServantApi Web.ApiRoutes))
  putStrLn "Done!"

elmImports :: Text
elmImports =
  T.unlines
    [ "import Json.Decode",
      "import Json.Encode exposing (Value)",
      "-- The following module comes from bartavelle/json-helpers",
      "import Json.Helpers exposing (..)",
      "import Dict", -- exposing (Dict)",
      -- "import Set",
      "import Http",
      "import String",
      "import Url.Builder"
    ]

myElmToString :: EType -> Text
myElmToString argType =
  case Debug.traceShowId argType of
    ETyCon (ETCon "UserID") -> "(\\(UserID id) -> id)"
    ETyApp (ETyCon (ETCon "List")) v -> "((List.map " <> myElmToString v <> ") >> String.join \",\")"
    _ -> defaultElmToString argType
