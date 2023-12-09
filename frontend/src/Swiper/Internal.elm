module Swiper.Internal exposing (..)

import Html
import Html.Attributes as Html
import List exposing (concatMap)



-- Attributes


type ContainerAttribute msg
    = CAttribute (Html.Attribute msg)
    | CAttributes (List (ContainerAttribute msg))


attribute : String -> String -> ContainerAttribute msg
attribute name val =
    CAttribute <| Html.attribute name val


toHtmlAttributes : List (ContainerAttribute msg) -> List (Html.Attribute msg)
toHtmlAttributes =
    concatMap <|
        \ca ->
            case ca of
                CAttribute attr ->
                    [ attr ]

                CAttributes attrs ->
                    toHtmlAttributes attrs


booleanAttribute : String -> Bool -> ContainerAttribute msg
booleanAttribute name bool =
    attribute name <|
        if bool then
            "true"

        else
            "false"
