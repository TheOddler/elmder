module Images exposing (..)

import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes as SvgAttr


{-| Loading image.
Created with <https://loading.io/spinner/spinner/-spinner-preloader-ajax-loading-icon>
Converted using <https://html-to-elm.com/>
And then customized a bit
-}
loading : String -> Html msg
loading class =
    svg
        [ SvgAttr.style "margin: auto; background: rgb(241, 242, 243); display: block; shape-rendering: auto;"
        , SvgAttr.viewBox "0 0 100 100"
        , SvgAttr.preserveAspectRatio "xMidYMid"
        , SvgAttr.class class
        ]
        [ Svg.g
            [ SvgAttr.transform "rotate(0 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#e6261f"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.875s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(45 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#eb7532"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.75s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(90 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#f7d038"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.625s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(135 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#a3e048"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.5s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(180 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#49da9a"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.375s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(225 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#34bbe6"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.25s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(270 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#4355db"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "-0.125s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        , Svg.g
            [ SvgAttr.transform "rotate(315 50 50)"
            ]
            [ Svg.rect
                [ SvgAttr.x "47"
                , SvgAttr.y "24"
                , SvgAttr.rx "3"
                , SvgAttr.ry "6"
                , SvgAttr.width "6"
                , SvgAttr.height "12"
                , SvgAttr.fill "#d23be7"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "opacity"
                    , SvgAttr.values "1;0"
                    , SvgAttr.keyTimes "0;1"
                    , SvgAttr.dur "1s"
                    , SvgAttr.begin "0s"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        ]
