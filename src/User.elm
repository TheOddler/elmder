module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)


type alias User =
    { id : String
    , name : String
    , headerImage : String
    , description : String
    , relationshipStatus : RelationshipStatus
    , sections : List UserSection
    }


type RelationshipStatus
    = Single
    | Married
    | InRelationship


type UserSection
    = Generic { header : String, content : String }
    | Image { url : String, description : String }
    | QuestionAndAnswer { question : String, answer : String }


viewCard : List (Attribute msg) -> User -> Html msg
viewCard attributes user =
    card
        attributes
        [ img [ src user.headerImage, class "max-height-half-screen" ] []
            |> withOverlay
                [ class "full-width" ]
                [ class "match-content text-on-image" ]
                [ div [ class "larger-text" ] [ text user.name ]
                , div [] [ text user.description ]
                ]
        ]


viewProfile : User -> Html msg
viewProfile user =
    div
        [ class "masonry" ]
    <|
        viewCard [] user
            :: List.map viewUserSection user.sections


viewUserSection : UserSection -> Html msg
viewUserSection userSection =
    case userSection of
        Generic { header, content } ->
            card []
                [ div [ class "larger-text" ] [ text header ]
                , div [] [ text content ]
                ]

        Image { url, description } ->
            card []
                [ img [ src url, class "full-width max-height-half-screen" ] []
                , div []
                    [ text description ]
                ]

        QuestionAndAnswer { question, answer } ->
            card []
                [ div [ class "larger-text" ] [ text question ]
                , div [] [ text answer ]
                ]
