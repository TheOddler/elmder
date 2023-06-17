module Swiper exposing (..)

-- Helper functions for Swiper: https://swiperjs.com/element

import Html exposing (Html, node)
import Html.Attributes
import Swiper.Internal exposing (..)


container : List (ContainerAttribute msg) -> List (Slide msg) -> Html msg
container attrs slides =
    node "swiper-container" (toHtmlAttributes attrs) (List.map unSlide slides)


type SafeLoopStrategy
    = DuplicateSlides
    | DisableLoop
    | DisableMultiView


{-| Having multiple slides per view visible and enabling loop can be glitchy if you don't have enough slides.
This tries to solve that by checking if there are enough slides, and if not applying the chosen strategy.
I'm not sure yet which strategy makes most sense, maybe it depends on the specific case, or maybe one is clearly better.
-}
containerMultiViewSafeLoop : SafeLoopStrategy -> Float -> List (ContainerAttribute msg) -> List (Slide msg) -> Html msg
containerMultiViewSafeLoop strategy slidesPerViewCount attrs slides =
    let
        isSafe =
            -- Swiper's docs say to use `>=` but there's a tiny graphical glitch then still
            -- not too bad though, so might be worth it for allowing multi-view-loop slightly more often
            List.length slides > ceiling slidesPerViewCount * 2
    in
    if isSafe then
        container (loop True :: slidesPerView (Count slidesPerViewCount) :: attrs) slides

    else
        case strategy of
            DuplicateSlides ->
                container (loop True :: slidesPerView (Count slidesPerViewCount) :: attrs) <| List.concat [ slides, slides ]

            DisableLoop ->
                container (slidesPerView (Count slidesPerViewCount) :: attrs) slides

            DisableMultiView ->
                container (loop True :: attrs) slides


slide : List (Html msg) -> Slide msg
slide elements =
    Slide <| node "swiper-slide" [] elements


imgSlide : List (Html.Attribute msg) -> Slide msg
imgSlide imgAttrs =
    slide
        [ Html.img
            imgAttrs
            []
        ]



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
