module Swiper.Internal exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import List exposing (concatMap)



-- Slides


type Slide msg
    = Slide (Html msg)


unSlide : Slide msg -> Html msg
unSlide (Slide html) =
    html



-- Attributes


type ContainerAttribute msg
    = CAttribute (Html.Attribute msg)
    | CAttributes (List (Html.Attribute msg))


cAttribute : String -> String -> ContainerAttribute msg
cAttribute name val =
    CAttribute <| Html.attribute name val


toHtmlAttributes : List (ContainerAttribute msg) -> List (Html.Attribute msg)
toHtmlAttributes =
    concatMap <|
        \ca ->
            case ca of
                CAttribute attr ->
                    [ attr ]

                CAttributes attrs ->
                    attrs


booleanCAttribute : String -> Bool -> ContainerAttribute msg
booleanCAttribute name bool =
    cAttribute name <|
        if bool then
            "true"

        else
            "false"
