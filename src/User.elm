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
        [ img [ src user.headerImage ] []
        , p [] [ text user.name ]
        , p [] [ text user.id ]
        , p [] [ text user.description ]
        ]
