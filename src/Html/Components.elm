module Html.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


card :
    { headerImage : String
    , header : String
    , subHeader : Maybe String
    , content : Maybe (Html msg)
    }
    -> Html msg
card settings =
    div [ class "card" ]
        [ figure []
            [ img [ src settings.headerImage ] []
            , figcaption [] <|
                div [ class "header" ] [ text settings.header ]
                    :: (case settings.subHeader of
                            Nothing ->
                                []

                            Just subHeader ->
                                [ div [ class "sub-header" ] [ text subHeader ]
                                ]
                       )
            ]
        , case settings.content of
            Nothing ->
                text ""

            Just content ->
                div [ class "content" ] [ content ]
        ]


navbar :
    { buttons : List a
    , getIcon :
        a
        -> String -- should give the font-awesome icon name
    , onSelect : a -> msg
    , isSelected : a -> Bool
    }
    -> Html msg
navbar { buttons, getIcon, onSelect, isSelected } =
    let
        mkButton a =
            div
                [ onClick <| onSelect a
                , class <|
                    if isSelected a then
                        "button selected"

                    else
                        "button"
                ]
                [ i [ class <| getIcon a ] []
                ]
    in
    div [ class "navbar" ] <| List.map mkButton buttons
