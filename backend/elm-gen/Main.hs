{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Elm.Module (recAlterType)
import Elm.TyRep (ETCon (..))
import Servant
import Servant.Elm
import Servant.Foreign (Foreign, HasForeign (..))
import User qualified
import User.GenderIdentity (GenderIdentity)
import User.Impressions (Impression)
import Web qualified

-- Taken from https://hackage.haskell.org/package/servant-foreign-0.16/docs/src/Servant.Foreign.Internal.html#line-518
-- But that is only available on servant-foreign 0.16, which is not in any stackage lts yet
instance HasForeign lang ftype (ToServantApi r) => HasForeign lang ftype (NamedRoutes r) where
  type Foreign ftype (NamedRoutes r) = Foreign ftype (ToServantApi r)

  foreignFor lang ftype Proxy =
    foreignFor lang ftype (Proxy :: Proxy (ToServantApi r))

main :: IO ()
main = do
  putStrLn "Generating elm..."
  generateElmModuleWith
    ( let myTypeAlterations = \case
            ETyCon (ETCon "Int32") -> ETyCon (ETCon "Int")
            ETyCon (ETCon "Day") -> ETyCon (ETCon "String") -- TODO: Convert days to something better than string
            other -> defaultTypeAlterations other

          myElmToString :: EType -> Text
          myElmToString argType =
            case argType of
              ETyCon (ETCon "Impression") -> "urlPieceFromImpression"
              _ -> defaultElmToString argType
       in defElmOptions
            { urlPrefix = Dynamic,
              elmAlterations =
                recAlterType myTypeAlterations,
              elmTypeAlterations = myTypeAlterations,
              elmToString = myElmToString
            }
    )
    [ "Generated",
      "Backend"
    ]
    elmImports
    "../frontend/src"
    [ DefineElm (Proxy :: Proxy User.UserID),
      DefineElm (Proxy :: Proxy User.UserOverviewInfo),
      DefineElm (Proxy :: Proxy User.UserExtendedInfo),
      DefineElm (Proxy :: Proxy User.UserAllInfo),
      DefineElm (Proxy :: Proxy User.RelationshipStatus),
      DefineElm (Proxy :: Proxy GenderIdentity),
      DefineElm (Proxy :: Proxy User.ProfileSection),
      DefineElm (Proxy :: Proxy Impression)
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
      -- "import String",
      "import Url.Builder",
      "",
      mkUrlPieceFromEnum "Impression" (Proxy :: Proxy Impression)
    ]

mkUrlPieceFromEnum ::
  forall a.
  (Enum a, Bounded a, Show a, ToHttpApiData a) =>
  Text ->
  Proxy a ->
  Text
mkUrlPieceFromEnum elmTypeName _ =
  T.unlines $
    [ funcName <> " : " <> elmTypeName <> " -> String",
      funcName <> " enumVal =",
      "  case enumVal of"
    ]
      ++ map renderCase [minBound .. maxBound]
  where
    funcName = "urlPieceFrom" <> elmTypeName
    renderCase :: a -> Text
    renderCase x =
      T.unlines
        [ "    " <> pack (show x) <> " -> ",
          "      \"" <> toUrlPiece x <> "\""
        ]
