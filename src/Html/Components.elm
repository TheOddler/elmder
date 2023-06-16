module Html.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attributes children =
    div (class "card" :: attributes) children


withOverlay :
    List (Attribute msg)
    -> List (Html msg)
    -> Html msg
    -> Html msg
withOverlay overlayAttributes overlayElements base =
    div [ class "overlay-wrapper" ]
        [ base
        , div (class "overlay" :: overlayAttributes) overlayElements
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
