module User.DeriveElmAndJson where

import Elm.Derive (Options (..), deriveBoth)
import Language.Haskell.TH qualified as TH
import Servant.Elm (defaultOptions)

deriveOptions :: Options
deriveOptions = Servant.Elm.defaultOptions {unwrapUnaryRecords = True}

deriveElmAndJson :: TH.Name -> TH.Q [TH.Dec]
deriveElmAndJson = deriveBoth deriveOptions
