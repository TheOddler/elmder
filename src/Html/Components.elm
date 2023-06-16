module Html.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attributes children =
    div (class "card" :: attributes) children


imageWithOverlay :
    { src : String
    , attributes : List (Attribute msg)
    , imageAttributes : List (Attribute msg)
    , overlay : List (Html msg)
    , overlayAttributes : List (Attribute msg)
    }
    -> Html msg
imageWithOverlay { src, attributes, overlay, overlayAttributes, imageAttributes } =
    figure (class "image-with-overlay" :: attributes)
        [ img (Html.Attributes.src src :: imageAttributes) []
        , figcaption (class "overlay" :: overlayAttributes) overlay
        ]


type alias NavbarButton msg =
    { icon : String -- The font-awesome icon name, this should include the style (fa-regular/fa-solid/...) as not all are available for free
    , onSelect : msg
    , isSelected : Bool
    }


navbar :
    List (NavbarButton msg)
    -> Html msg
navbar buttons =
    let
        viewButton : NavbarButton msg -> Html msg
        viewButton btn =
            div
                [ onClick <| btn.onSelect
                , class <|
                    if btn.isSelected then
                        "button selected"

                    else
                        "button"
                ]
                [ i [ class <| btn.icon ] []
                ]
    in
    div [ class "navbar" ] <| List.map viewButton buttons
