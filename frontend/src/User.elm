module User exposing (..)

import Api.Data exposing (User, UserSection(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)
import Swiper


type alias UserID =
    String


viewCard : List (Attribute msg) -> User -> Html msg
viewCard attributes user =
    card
        attributes
        [ img [ src user.headerImage, class "full-width standard-height" ] []
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
        UserSectionUserSectionGeneric { header, content } ->
            card []
                [ div [ class "larger-text" ] [ text header ]
                , div [] [ text content ]
                ]

        UserSectionUserSectionImages { images, description } ->
            card []
                [ Swiper.containerMultiViewSafeLoop Swiper.DisableMultiView
                    1.2
                    [ Swiper.grabCursor True
                    , Swiper.pagination Swiper.ProgressBar
                    , Swiper.centeredSlides True
                    , Swiper.slideToClickedSlide True
                    ]
                    (let
                        viewSlide url =
                            Swiper.imgSlide
                                [ src url
                                , class "full-width standard-height"
                                ]
                     in
                     List.map viewSlide images
                    )
                    |> withOverlay [ class "full-width" ] [ class "match-content text-on-image" ] [ text description ]
                ]

        UserSectionUserSectionQuestionAndAnswer { question, answer } ->
            card []
                [ div [ class "larger-text" ] [ text question ]
                , div [] [ text answer ]
                ]
