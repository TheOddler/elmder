module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)
import Html.Events exposing (onClick)


type alias User =
    { id : String
    , name : String
    , headerImage : String
    , description : String
    , relationshipStatus : RelationshipStatus
    , section : List UserSection
    }


type RelationshipStatus
    = Single
    | Married
    | InRelationship


type UserSection
    = Generic { header : String, content : String }
    | Image { url : String, description : String }
    | QuestionAndAnswer { question : String, answer : String }


viewCard : (User -> msg) -> User -> Html msg
viewCard onClickUser user =
    card
        [ onClick <| onClickUser user ]
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


viewProfileOverlay : msg -> User -> Html msg
viewProfileOverlay close user =
    div
        [ class "full-screen-overlay"
        , class "center-content"
        , onClick close
        ]
        [ card
            []
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
        ]
