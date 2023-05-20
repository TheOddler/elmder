module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (card)


type alias User =
    { id : String
    , name : String
    , headerImage : String
    , description : String
    }


viewCard : User -> Html msg
viewCard user =
    card
        { headerImage = user.headerImage
        , header = user.name
        , subHeader = Just user.description
        , content = Nothing -- Just <| p [] [ text "test" ]
        }
