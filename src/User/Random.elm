module User.Random exposing (..)

import Random exposing (..)
import Random.Char exposing (..)
import Random.Extra exposing (..)
import Random.String exposing (..)
import User exposing (User)


random : Generator User
random =
    let
        imgUrl rnd =
            "http://placekitten.com/200/300?" ++ String.fromInt rnd
    in
    map User (string 10 ascii)
        |> andMap (rangeLengthString 5 15 english)
        |> andMap (map imgUrl (int minInt maxInt))
        |> andMap (rangeLengthString 50 500 english)
