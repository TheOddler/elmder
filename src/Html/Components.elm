module Html.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


card :
    { headerImage : String
    , header : String
    , subHeader : Maybe String
    , content : Maybe (Html msg)
    }
    -> Html msg
card settings =
    div [ class "card" ]
        [ figure []
            [ img [ src settings.headerImage ] []
            , figcaption [] <|
                div [ class "header" ] [ text settings.header ]
                    :: (case settings.subHeader of
                            Nothing ->
                                []

                            Just subHeader ->
                                [ div [ class "sub-header" ] [ text subHeader ]
                                ]
                       )
            ]
        , case settings.content of
            Nothing ->
                text ""

            Just content ->
                div [ class "content" ] [ content ]
        ]
