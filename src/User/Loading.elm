module User.Loading exposing (..)

import Html exposing (Attribute, Html)
import Html.Components exposing (card)
import Images
import List.Extra as List
import User exposing (UserID)
import User.Store exposing (RequestedUserID, unRequestUserID)


cssColor : UserID -> String
cssColor id =
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
            String.toList id
                |> List.map Char.toCode
                |> List.sum

        index =
            modBy (List.length colors) random
    in
    List.getAt index colors
        |> Maybe.withDefault "#000000"


viewCardLoading : List (Attribute msg) -> RequestedUserID -> Html msg
viewCardLoading attributes userID =
    card
        attributes
        [ Images.heart "full-width standard-height" <| cssColor (unRequestUserID userID)
        ]
