module Util where

import Data.List (find)
import Data.Text (Text)

reverseEnumToText :: (Enum a, Bounded a) => (a -> Text) -> Text -> Maybe a
reverseEnumToText enumToText text =
  flip find [minBound .. maxBound] $ \t ->
    enumToText t == text
