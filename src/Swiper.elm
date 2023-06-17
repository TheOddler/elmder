module Swiper exposing (..)

-- Helper functions for Swiper: https://swiperjs.com/element

import Html exposing (Html, node)
import Html.Attributes
import Swiper.Internal exposing (..)


container : List (ContainerAttribute msg) -> List (Slide msg) -> Html msg
container attr slides =
    node "swiper-container" (toHtmlAttributes attr) (List.map unSlide slides)


slide : List (Html msg) -> Slide msg
slide elements =
    Slide <| node "swiper-slide" [] elements



-- Attributes


class : String -> ContainerAttribute msg
class className =
    CAttribute (Html.Attributes.class className)


type SliderPerView
    = Auto
    | Count Float


slidesPerView : SliderPerView -> ContainerAttribute msg
slidesPerView count =
    cAttribute "slides-per-view" <|
        case count of
            Auto ->
                "auto"

            Count c ->
                String.fromFloat c


centeredSlides : Bool -> ContainerAttribute msg
centeredSlides =
    booleanCAttribute "centered-slides"


type SpaceBetween
    = Px Int
    | Other String


spaceBetween : SpaceBetween -> ContainerAttribute msg
spaceBetween space =
    cAttribute "space-between" <|
        case space of
            Px px ->
                String.fromInt px

            Other other ->
                other


navigation : Bool -> ContainerAttribute msg
navigation =
    booleanCAttribute "navigation"


type PaginationType
    = Bullets
    | Fraction
    | ProgressBar
      -- The following two are a bit special, but I consider them kinds of pagination
    | DynamicBullets
    | Scrollbar


pagination : PaginationType -> ContainerAttribute msg
pagination type_ =
    let
        useTypeAttrSimple =
            useTypeAttr "pagination-type"

        useTypeAttr typeAttrName typeString =
            CAttributes
                [ Html.Attributes.attribute "pagination" "true"
                , Html.Attributes.attribute typeAttrName typeString
                ]
    in
    case type_ of
        Bullets ->
            useTypeAttrSimple "bullets"

        Fraction ->
            useTypeAttrSimple "fraction"

        ProgressBar ->
            useTypeAttrSimple "progressbar"

        DynamicBullets ->
            -- Dynamic Bullets for some reason does not use the normal "pagination-type" attribute
            useTypeAttr "pagination-dynamic-bullets" "true"

        Scrollbar ->
            -- Scrollbar is not considered pagination at all, but I think it serves the same purpose
            cAttribute "scrollbar" "true"


{-| Because of nature of how the loop mode works (it will rearrange slides), total number of slides must be >= slidesPerView \* 2
Default: False
-}
loop : Bool -> ContainerAttribute msg
loop =
    booleanCAttribute "loop"


autoHeight : Bool -> ContainerAttribute msg
autoHeight =
    booleanCAttribute "auto-height"


{-| This option may a little improve desktop usability. If true, user will see the "grab" cursor when hover on Swiper
Default: False
-}
grabCursor : Bool -> ContainerAttribute msg
grabCursor =
    booleanCAttribute "grab-cursor"


{-| Set to true to round values of slides width and height to prevent blurry texts on usual resolution screens (if you have such)
Default: False
-}
roundLengths : Bool -> ContainerAttribute msg
roundLengths =
    booleanCAttribute "round-lengths"


{-| Set to true and click on any slide will produce transition to this slide
Default: False
-}
slideToClickedSlide : Bool -> ContainerAttribute msg
slideToClickedSlide =
    booleanCAttribute "slide-to-clicked-slide"
