module Swiper exposing (..)

-- Helper functions for Swiper: https://swiperjs.com/element

import Html exposing (Html, node)
import Html.Attributes
import Swiper.Internal exposing (..)


type Slide msg
    = Slide (Html msg)


container : List (ContainerAttribute msg) -> List (Slide msg) -> Html msg
container attrs slides =
    let
        allAttrs =
            -- We need to set swiper to observe,
            -- otherwise it won't play nice with Elm
            booleanAttribute "observer" True
                :: booleanAttribute "observe-parents" True
                :: booleanAttribute "observe-slide-children" True
                :: attrs

        unSlide : Slide msg -> Html msg
        unSlide (Slide html) =
            html
    in
    node "swiper-container" (toHtmlAttributes allAttrs) (List.map unSlide slides)


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


slideWithClass : String -> List (Html msg) -> Slide msg
slideWithClass className elements =
    Slide <| node "swiper-slide" [ Html.Attributes.class className ] elements


imgSlide : List (Html.Attribute msg) -> Slide msg
imgSlide imgAttrs =
    slide
        [ Html.img
            imgAttrs
            []
        ]



-- Attributes


id : String -> ContainerAttribute msg
id =
    CAttribute << Html.Attributes.id


class : String -> ContainerAttribute msg
class =
    CAttribute << Html.Attributes.class


type SliderPerView
    = Auto
    | Count Float


slidesPerView : SliderPerView -> ContainerAttribute msg
slidesPerView count =
    attribute "slides-per-view" <|
        case count of
            Auto ->
                "auto"

            Count c ->
                String.fromFloat c


centeredSlides : Bool -> ContainerAttribute msg
centeredSlides =
    booleanAttribute "centered-slides"


type SpaceBetween
    = Px Int
    | Other String


spaceBetween : SpaceBetween -> ContainerAttribute msg
spaceBetween space =
    attribute "space-between" <|
        case space of
            Px px ->
                String.fromInt px

            Other other ->
                other


navigation : Bool -> ContainerAttribute msg
navigation =
    booleanAttribute "navigation"


type PaginationBulletKind
    = Dynamic
    | Standard


type PaginationType
    = Bullets PaginationBulletKind
    | Fraction
    | ProgressBar


pagination : PaginationType -> ContainerAttribute msg
pagination type_ =
    let
        useTypeAttr typeString =
            CAttributes
                [ booleanAttribute "pagination" True
                , attribute "pagination-type" typeString
                ]
    in
    case type_ of
        Bullets kind ->
            case kind of
                Dynamic ->
                    CAttributes
                        [ booleanAttribute "pagination-dynamic-bullets" True
                        , useTypeAttr "bullets"
                        ]

                Standard ->
                    useTypeAttr "bullets"

        Fraction ->
            useTypeAttr "fraction"

        ProgressBar ->
            useTypeAttr "progressbar"


type SrollbarHide
    = AutoHide
    | AlwaysVisible


scrollbar : SrollbarHide -> ContainerAttribute msg
scrollbar hide =
    CAttributes
        [ booleanAttribute "scrollbar" True
        , booleanAttribute "scrollbar-hide" (hide == AutoHide)
        ]


{-| Because of nature of how the loop mode works (it will rearrange slides), total number of slides must be >= slidesPerView \* 2
Default: False
-}
loop : Bool -> ContainerAttribute msg
loop =
    booleanAttribute "loop"


autoHeight : Bool -> ContainerAttribute msg
autoHeight =
    booleanAttribute "auto-height"


{-| This option may a little improve desktop usability. If true, user will see the "grab" cursor when hover on Swiper
Default: False
-}
grabCursor : Bool -> ContainerAttribute msg
grabCursor =
    booleanAttribute "grab-cursor"


{-| Set to true to round values of slides width and height to prevent blurry texts on usual resolution screens (if you have such)
Default: False
-}
roundLengths : Bool -> ContainerAttribute msg
roundLengths =
    booleanAttribute "round-lengths"


{-| Set to true and click on any slide will produce transition to this slide
Default: False
-}
slideToClickedSlide : Bool -> ContainerAttribute msg
slideToClickedSlide =
    booleanAttribute "slide-to-clicked-slide"


{-| When enabled Swiper will be disabled and hide navigation buttons on case there are not enough slides for sliding.
Default: True
-}
watchOverflow : Bool -> ContainerAttribute msg
watchOverflow =
    booleanAttribute "watch-overflow"


type Effect
    = EffectSlide
    | EffectFade
    | EffectCube
    | EffectCoverflow
    | EffectFlip
    | EffectCreative
    | EffectCards


effect : Effect -> ContainerAttribute msg
effect kind =
    attribute "effect" <|
        case kind of
            EffectSlide ->
                "slide"

            EffectFade ->
                "fade"

            EffectCube ->
                "cube"

            EffectCoverflow ->
                "coverflow"

            EffectFlip ->
                "flip"

            EffectCreative ->
                "creative"

            EffectCards ->
                "cards"
