module Enums exposing (..)

import Generated.Backend exposing (Impression(..))
import List.Extra as List


reverseEnumToString : List a -> (a -> String) -> String -> Maybe a
reverseEnumToString allEnumValues enumToString string =
    List.find (\str -> enumToString str == string) allEnumValues
