module Swiper.Config exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


type SliderPerView
    = Auto
    | Count Float


slidesPerView : SliderPerView -> Attribute msg
slidesPerView count =
    attribute "slides-per-view" <|
        case count of
            Auto ->
                "auto"

            Count c ->
                String.fromFloat c


centeredSlides : Bool -> Attribute msg
centeredSlides =
    booleanConfig "centered-slides"


type SpaceBetween
    = Px Int
    | Other String


spaceBetween : SpaceBetween -> Attribute msg
spaceBetween space =
    attribute "space-between" <|
        case space of
            Px px ->
                String.fromInt px

            Other other ->
                other


navigation : Bool -> Attribute msg
navigation =
    booleanConfig "navigation"


pagination : Bool -> Attribute msg
pagination =
    booleanConfig "pagination"


type PaginationType
    = Bullets
    | Fraction
    | ProgressBar
    | DynamicBullets


paginationType : PaginationType -> Attribute msg
paginationType type_ =
    let
        useTypeAttr =
            attribute "pagination-type"
    in
    case type_ of
        Bullets ->
            useTypeAttr "bullets"

        Fraction ->
            useTypeAttr "fraction"

        ProgressBar ->
            useTypeAttr "progressbar"

        DynamicBullets ->
            attribute "pagination-dynamic-bullets" "true"


scrollbar : Bool -> Attribute msg
scrollbar =
    booleanConfig "scrollbar"


{-| Because of nature of how the loop mode works (it will rearrange slides), total number of slides must be >= slidesPerView \* 2
Default: False
-}
loop : Bool -> Attribute msg
loop =
    booleanConfig "loop"


autoHeight : Bool -> Attribute msg
autoHeight =
    booleanConfig "auto-height"


{-| This option may a little improve desktop usability. If true, user will see the "grab" cursor when hover on Swiper
Default: False
-}
grabCursor : Bool -> Attribute msg
grabCursor =
    booleanConfig "grab-cursor"


{-| Set to true to round values of slides width and height to prevent blurry texts on usual resolution screens (if you have such)
Default: False
-}
roundLengths : Bool -> Attribute msg
roundLengths =
    booleanConfig "round-lengths"


{-| Set to true and click on any slide will produce transition to this slide
Default: False
-}
slideToClickedSlide : Bool -> Attribute msg
slideToClickedSlide =
    booleanConfig "slide-to-clicked-slide"


booleanConfig : String -> Bool -> Attribute msg
booleanConfig name bool =
    attribute name <|
        if bool then
            "true"

        else
            "false"
