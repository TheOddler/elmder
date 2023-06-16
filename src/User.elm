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
        [ img [ src user.headerImage, class "full-width max-height-half-screen" ] []
            |> withOverlay [ class "content text-on-image" ]
                [ div [ class "larger-text" ] [ text user.name ]
                , div [] [ text user.description ]
                ]
        ]


viewProfile : User -> Html msg
viewProfile user =
    div
        []
        [ card
            []
          <|
            (img [ src user.headerImage, class "full-width max-height-half-screen" ] []
                |> withOverlay [ class "content text-on-image" ]
                    [ div [ class "larger-text" ] [ text user.name ]
                    , div [] [ text user.description ]
                    ]
            )
                :: List.map viewUserSection user.sections
        ]


viewUserSection : UserSection -> Html msg
viewUserSection userSection =
    case userSection of
        Generic { header, content } ->
            div []
                [ div [ class "larger-text" ] [ text header ]
                , div [] [ text content ]
                ]

        Image { url, description } ->
            img [ src url, class "full-width max-height-half-screen" ] []
                |> withOverlay [ class "content text-on-image" ]
                    [ text description
                    ]

        QuestionAndAnswer { question, answer } ->
            div []
                [ div [ class "larger-text" ] [ text question ]
                , div [] [ text answer ]
                ]
