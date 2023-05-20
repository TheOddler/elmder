module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias User =
    { id : String
    , name : String
    , headerImage : String
    , description : String
    }


viewCard : User -> Html msg
viewCard user =
    div [ class "card" ]
        [ div [ class "card=image" ]
            [ figure [ class "image is-4by3" ]
                [ img [ src user.headerImage ]
                    []
                ]
            ]
        , div [ class "card-content" ]
            [ div [ class "media" ]
                [ div [ class "media-left" ]
                    [ figure [ class "image is-48x48" ]
                        [ img [ src user.headerImage ] [] ]
                    ]
                , div [ class "media-content" ]
                    [ p [ class "title is-4" ]
                        [ text user.name ]
                    , p
                        [ class "subtitle is-6" ]
                        [ text user.id ]
                    ]
                ]
            , div [ class "content" ]
                [ text user.description
                ]
            ]
        ]
