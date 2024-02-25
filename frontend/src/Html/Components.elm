module Html.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attributes children =
    div (class "card" :: attributes) children


withOverlay :
    List (Attribute msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
    -> Html msg
withOverlay wrapperAttrs overlayAttrs overlayElements base =
    div (class "with-overlay" :: wrapperAttrs)
        [ base
        , div (class "overlay" :: overlayAttrs) overlayElements
        ]


type alias NavbarButton msg =
    { icon : String -- The font-awesome icon name, this should include the style (fa-regular/fa-solid/...) as not all are available for free
    , onSelect : msg
    , isSelected : Bool
    , isVisible : Bool -- Used to hide buttons that are not available, but we still want them in the dom so we can do animations on them
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
                , classList
                    [ ( "button", True )
                    , ( "selected", btn.isSelected )
                    , ( "hidden", not btn.isVisible )
                    ]
                ]
                [ i [ class <| btn.icon ] []
                ]
    in
    div
        [ id "navbar" ]
    <|
        List.map viewButton buttons
