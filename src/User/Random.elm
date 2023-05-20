module User.Random exposing (..)

import Faker exposing (..)
import Random exposing (..)
import Random.Char exposing (..)
import Random.Extra exposing (..)
import Random.String exposing (..)
import User exposing (User)


random : Generator User
random =
    let
        id =
            string 10 lowerCaseLatin
    in
    map User id
        |> andMap name
        |> andMap imgUrl
        |> andMap paragraph
