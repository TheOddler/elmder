module User.Loading exposing (..)

import Html exposing (Attribute, Html)
import Html.Components exposing (card)
import Images
import List.Extra as List
import Store exposing (Requested, unRequest)
import User exposing (UserID)


cssColor : UserID -> String
cssColor userID =
    let
        colors : List String
        colors =
            [ "#e6261f" -- Red
            , "#eb7532" -- Orange
            , "#f7d038" -- Yellow
            , "#a3e048" -- Green
            , "#49da9a" -- Geen Blue
            , "#34bbe6" -- Light Blue
            , "#4355db" -- Blue
            , "#d23be7" -- Purple
            ]

        random : Int
        random =
            -- Random enough assuming the id is random
            String.toList userID
                |> List.map Char.toCode
                |> List.sum

        index =
            modBy (List.length colors) random
    in
    List.getAt index colors
        |> Maybe.withDefault "#000000"


viewCardLoading : List (Attribute msg) -> Requested UserID -> Html msg
viewCardLoading attributes userID =
    card
        attributes
        [ Images.heart "full-width standard-height" <| cssColor (unRequest userID)
        ]
