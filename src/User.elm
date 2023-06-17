module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)
import Swiper
import Swiper.Config as Swiper


type alias User =
    { id : String
    , name : String
    , images : List String
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
        [ case user.images of
            first :: _ ->
                img [ src first, class "max-height-half-screen" ] []
                    |> withOverlay
                        [ class "full-width" ]
                        [ class "match-content text-on-image" ]
                        [ div [ class "larger-text" ] [ text user.name ]
                        , div [] [ text user.description ]
                        ]

            [] ->
                div [] []
        ]


viewProfile : User -> Html msg
viewProfile user =
    div
        [ class "masonry" ]
    <|
        viewCard [] user
            :: card []
                [ Swiper.container
                    [ class "full-width"

                    -- , Swiper.loop True -- loop is glitchy combined with slides per view if there aren't enough slides
                    , Swiper.grabCursor True
                    , Swiper.navigation True
                    , Swiper.pagination True
                    , Swiper.paginationType Swiper.ProgressBar
                    , Swiper.slidesPerView (Swiper.Count 1.2)
                    , Swiper.centeredSlides True
                    , Swiper.slideToClickedSlide True
                    ]
                  <|
                    List.map (\url -> Swiper.slide [ img [ src url, class "full-width" ] [] ]) user.images
                ]
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
