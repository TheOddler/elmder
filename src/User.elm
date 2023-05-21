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


viewProfile : User -> Html msg
viewProfile user =
    div
        []
        [ card
            []
          <|
            imageWithOverlay
                { image = user.headerImage
                , attributes = [ class "full-width" ]
                , overlay =
                    [ div [ class "larger-text text-on-image" ] [ text user.name ]
                    , div [ class "text-on-image" ] [ text user.description ]
                    ]
                , overlayAttributes = [ class "content" ]
                }
                :: List.map viewUserSection user.sections
        ]


viewUserSection : UserSection -> Html msg
viewUserSection userSection =
    case userSection of
        Generic { header, content } ->
            div [ class "content" ]
                [ div [ class "larger-text" ] [ text header ]
                , div [] [ text content ]
                ]

        Image { url, description } ->
            imageWithOverlay
                { image = url
                , attributes = [ class "full-width" ]
                , overlay =
                    [ div [ class "text-on-image" ] [ text description ]
                    ]
                , overlayAttributes = [ class "content" ]
                }

        QuestionAndAnswer { question, answer } ->
            div [ class "content" ]
                [ div [ class "larger-text" ] [ text question ]
                , div [] [ text answer ]
                ]
