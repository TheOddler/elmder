module User exposing (..)

import Generated.Backend exposing (Impression(..), ProfileSection(..), UserExtendedInfo, UserID, UserOverviewInfo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Components exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickStopPropagation)
import StringExtra as String
import Swiper


type alias UserWithImpression =
    { user : UserOverviewInfo
    , impression : Maybe Impression
    }


viewCard : (UserID -> Impression -> msg) -> List (Attribute msg) -> UserWithImpression -> Html msg
viewCard setImpression attributes { user, impression } =
    card
        attributes
        [ Html.img
            [ src user.userHeaderImageUrl
            ]
            []
        , div [ class "content text-on-image" ]
            [ div [ class "x-large-text" ]
                [ text <| user.userName
                ]
            , let
                infoRow icon t =
                    tr []
                        [ td [] [ i [ class icon ] [] ]
                        , td [] [ text t ]
                        ]
              in
              table [ class "icon-table" ]
                [ infoRow "fa-solid fa-user" <| String.fromGenderIdentity user.userGenderIdentity ++ " • " ++ String.fromInt user.userAge
                , infoRow "fa-solid fa-location-dot" <| String.fromInt user.userDistanceM ++ "m away"
                ]
            , let
                impressionBtn icon className impr =
                    button
                        [ class className
                        , class <|
                            if impression == Just impr then
                                "selected"

                            else
                                ""
                        , onClickStopPropagation <| setImpression user.userId impr
                        ]
                        [ i [ class icon ] [] ]
              in
              div [ class "impressions" ]
                [ impressionBtn
                    "fa-solid fa-xmark"
                    "dislike"
                    ImpressionDislike
                , impressionBtn
                    "fa-solid fa-heart"
                    "like"
                    ImpressionLike
                , impressionBtn
                    "fa-regular fa-clock"
                    "decide-later"
                    ImpressionDecideLater
                ]
            ]
        ]


viewProfile :
    (UserID -> Impression -> msg)
    -> UserOverviewInfo
    -> UserExtendedInfo
    -> Html msg
viewProfile setImpression userInfo extendedInfo =
    div
        [ class "masonry" ]
    <|
        viewCard setImpression [] { user = userInfo, impression = Nothing }
            :: button [ onClick <| setImpression userInfo.userId ImpressionLike ]
                [ text "Like "
                , i [ class "fa-solid fa-heart" ] []
                ]
            :: button [ onClick <| setImpression userInfo.userId ImpressionDislike ]
                [ text "Dislike "
                , i [ class "fa-solid fa-heart-crack" ] []
                ]
            :: button [ onClick <| setImpression userInfo.userId ImpressionDecideLater ]
                [ text "Decide later "
                , i [ class "fa-solid fa-clock" ] []
                ]
            :: List.map viewUserSection extendedInfo.userExtProfileSections


viewUserSection : ProfileSection -> Html msg
viewUserSection section =
    case section of
        UserSectionGeneric { userSectionGenericHeader, userSectionGenericContent } ->
            card []
                [ div [ class "larger-text" ] [ text userSectionGenericHeader ]
                , div [] [ text userSectionGenericContent ]
                ]

        UserSectionImages { userSectionImagesImages, userSectionImagesDescription } ->
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
                     List.map viewSlide userSectionImagesImages
                    )
                    |> withOverlay [ class "full-width" ] [ class "match-content text-on-image" ] [ text userSectionImagesDescription ]
                ]

        UserSectionQuestionAndAnswer { userSectionQuestionAndAnswerQuestion, userSectionQuestionAndAnswerAnswer } ->
            card []
                [ div [ class "larger-text" ] [ text userSectionQuestionAndAnswerQuestion ]
                , div [] [ text userSectionQuestionAndAnswerAnswer ]
                ]
