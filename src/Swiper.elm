module Swiper exposing (container, slide)

-- Helper functions for Swiper: https://swiperjs.com/element

import Html exposing (Attribute, Html, node)


container : List (Attribute msg) -> List (Slide msg) -> Html msg
container attr slides =
    node "swiper-container" attr <| List.map unSlide slides


type Slide msg
    = Slide (Html msg)


unSlide : Slide msg -> Html msg
unSlide (Slide html) =
    html


slide : List (Html msg) -> Slide msg
slide elements =
    Slide <| node "swiper-slide" [] elements
