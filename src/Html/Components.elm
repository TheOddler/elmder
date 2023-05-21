module Html.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


card : List (Html msg) -> Html msg
card children =
    div [ class "card" ] children


imageWithOverlay :
    { image : String
    , attributes : List (Attribute msg)
    , overlay : List (Html msg)
    , overlayAttributes : List (Attribute msg)
    }
    -> Html msg
imageWithOverlay { image, attributes, overlay, overlayAttributes } =
    figure (class "image-with-overlay" :: attributes)
        [ img [ src image ] []
        , figcaption (class "overlay" :: overlayAttributes) overlay
        ]


navbar :
    { buttons : List a
    , getIcon :
        a
        -> String -- should give the font-awesome icon name
    , onSelect : a -> msg
    , selected : a
    }
    -> Html msg
navbar { buttons, getIcon, onSelect, selected } =
    let
        mkButton a =
            div
                [ onClick <| onSelect a
                , class <|
                    if selected == a then
                        "button selected"

                    else
                        "button"
                ]
                [ i [ class <| getIcon a ] []
                ]
    in
    div [ class "navbar" ] <| List.map mkButton buttons
