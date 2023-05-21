module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)


type alias User =
    { id : String
    , name : String
    , headerImage : String
    , description : String
    }


viewCard : User -> Html msg
viewCard user =
    card
        [ imageWithOverlay
            { image = user.headerImage
            , attributes = [ class "full-width" ]
            , overlay =
                [ div [ class "larger-text text-on-image" ] [ text user.name ]
                , div [ class "text-on-image" ] [ text user.description ]
                ]
            , overlayAttributes = [ class "content" ]
            }
        ]
