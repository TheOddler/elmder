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


type alias UserInteractions msg =
    { setImpression : UserID -> Impression -> msg
    , viewProfile : UserID -> msg
    }


viewCard : UserInteractions msg -> List (Attribute msg) -> UserWithImpression -> Html msg
viewCard interactions attributes userWithImpression =
    card
        attributes
        (viewCardContent interactions userWithImpression)


viewCardAsSwiperSlide : UserInteractions msg -> UserWithImpression -> Swiper.Slide msg
viewCardAsSwiperSlide interactions userWithImpression =
    Swiper.slideWithClass "card"
        (viewCardContent interactions userWithImpression)


type IncludeImpressionButtons
    = LikeDisslikeLater
    | NoButtons


viewCardContent : UserInteractions msg -> UserWithImpression -> List (Html msg)
viewCardContent interactions { user, impression } =
    [ Html.img
        [ src user.userHeaderImageUrl
        , onClickStopPropagation <| interactions.viewProfile user.userId
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
                    , onClickStopPropagation <| interactions.setImpression user.userId impr
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
    UserInteractions msg
    -> UserExtendedInfo
    -> Html msg
viewProfile interactions extendedInfo =
    div
        [ class "masonry scrollable" ]
    <|
        List.map viewUserSection extendedInfo.userExtProfileSections



-- TODO: Add this back
-- viewCard interactions [] { user = userInfo, impression = Nothing }
--     :: button [ onClick <| interactions.setImpression userInfo.userId ImpressionLike ]
--         [ text "Like "
--         , i [ class "fa-solid fa-heart" ] []
--         ]
--     :: button [ onClick <| interactions.setImpression userInfo.userId ImpressionDislike ]
--         [ text "Dislike "
--         , i [ class "fa-solid fa-heart-crack" ] []
--         ]
--     :: button [ onClick <| interactions.setImpression userInfo.userId ImpressionDecideLater ]
--         [ text "Decide later "
--         , i [ class "fa-solid fa-clock" ] []
--         ]
--     :: List.map viewUserSection extendedInfo.userExtProfileSections


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
                    , Swiper.pagination (Swiper.Bullets Swiper.Dynamic)
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
