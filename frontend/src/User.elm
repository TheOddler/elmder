module User exposing (..)

import Generated.Backend exposing (ProfileSection(..), User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)
import Swiper


viewCard : List (Attribute msg) -> User -> Html msg
viewCard attributes user =
    card
        attributes
        [ img [ src user.userHeaderImage, class "full-width standard-height" ] []
            |> withOverlay
                [ class "full-width" ]
                [ class "match-content text-on-image" ]
                [ div [ class "larger-text" ] [ text user.userName ]
                , div [] [ text user.userDescription ]
                ]
        ]


viewProfile : User -> Html msg
viewProfile user =
    div
        [ class "masonry" ]
    <|
        viewCard [] user
            -- :: List.map viewUserSection user.userSections
            :: []



-- viewUserSection : UserSection -> Html msg
-- viewUserSection userSection =
--     case userSection of
--         UserSectionGeneric { userSectionGenericHeader, userSectionGenericContent } ->
--             card []
--                 [ div [ class "larger-text" ] [ text userSectionGenericHeader ]
--                 , div [] [ text userSectionGenericContent ]
--                 ]
--         UserSectionImages { userSectionImagesImages, userSectionImagesDescription } ->
--             card []
--                 [ Swiper.containerMultiViewSafeLoop Swiper.DisableMultiView
--                     1.2
--                     [ Swiper.grabCursor True
--                     , Swiper.pagination Swiper.ProgressBar
--                     , Swiper.centeredSlides True
--                     , Swiper.slideToClickedSlide True
--                     ]
--                     (let
--                         viewSlide url =
--                             Swiper.imgSlide
--                                 [ src url
--                                 , class "full-width standard-height"
--                                 ]
--                      in
--                      List.map viewSlide userSectionImagesImages
--                     )
--                     |> withOverlay [ class "full-width" ] [ class "match-content text-on-image" ] [ text userSectionImagesDescription ]
--                 ]
--         UserSectionQuestionAndAnswer { userSectionQuestionAndAnswerQuestion, userSectionQuestionAndAnswerAnswer } ->
--             card []
--                 [ div [ class "larger-text" ] [ text userSectionQuestionAndAnswerQuestion ]
--                 , div [] [ text userSectionQuestionAndAnswerAnswer ]
--                 ]
