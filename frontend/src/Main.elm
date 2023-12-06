module Main exposing (..)

import Browser
import Either exposing (Either(..))
import Generated.Backend as Backend exposing (Impression(..), UserExtendedInfo, UserID, UserOverviewInfo, jsonEncImpression)
import Html exposing (Html, a, button, div, h1, i, table, td, text, tr)
import Html.Attributes exposing (class, href, src)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Http
import Json.Encode exposing (encode)
import Ports exposing (swiperSlideNext)
import StringExtra as String
import Swiper
import User


type NavButton
    = NavButtonSearch
    | NavButtonImpressions
    | NavButtonMyProfile


allNavButtons : List NavButton
allNavButtons =
    [ NavButtonSearch
    , NavButtonImpressions
    , NavButtonMyProfile
    ]


allImpressions : List Impression
allImpressions =
    [ ImpressionLike, ImpressionDislike, ImpressionDecideLater, ImpressionSuperLike ]


type alias UserWithImpression =
    { user : UserOverviewInfo
    , impression : Maybe Impression
    }


type Screen
    = ScreenSearch (List UserWithImpression)
    | ScreenImpression Impression (List UserOverviewInfo)
    | ScreenMyProfile
    | ScreenOtherUser UserOverviewInfo UserExtendedInfo


type alias Model =
    { settings : AppSettings
    , currentScreen : Screen
    , lastClickedNavButton : NavButton
    , unrecoverableError : Maybe String
    }


type Msg
    = ClickedNavButton NavButton
    | OpenScreen Screen
    | OpenScreenImpression Impression
    | ViewUser UserOverviewInfo
    | GotUnrecoverableErrror String
    | SetUserImpression UserID Impression
    | GotSetUserImpressionResult (Result Http.Error ())


type alias AppSettings =
    { backendUrl : String
    }


main : Program AppSettings Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : AppSettings -> ( Model, Cmd Msg )
init settings =
    ( { currentScreen = ScreenSearch []
      , lastClickedNavButton = NavButtonSearch
      , unrecoverableError = Nothing
      , settings = settings
      }
    , openScreenSearch settings
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


openScreenSearch : AppSettings -> Cmd Msg
openScreenSearch settings =
    Backend.getUserSearch settings.backendUrl
        (\errorOrUsers ->
            case errorOrUsers of
                Err err ->
                    GotUnrecoverableErrror <| "Failed searching for users: " ++ httpErrorToString err

                Ok users ->
                    OpenScreen <|
                        ScreenSearch <|
                            List.map (\u -> { user = u, impression = Nothing })
                                users
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotUnrecoverableErrror error ->
            ( { model | unrecoverableError = Just error }, Cmd.none )

        ClickedNavButton button ->
            let
                ( newScreen, cmds ) =
                    case button of
                        NavButtonSearch ->
                            ( ScreenSearch []
                            , openScreenSearch model.settings
                            )

                        NavButtonImpressions ->
                            ( ScreenImpression ImpressionLike []
                            , Backend.getUserImpressionsByImpression model.settings.backendUrl
                                ImpressionLike
                                (\errorOrUsers ->
                                    case errorOrUsers of
                                        Err err ->
                                            GotUnrecoverableErrror <| "Failed getting liked users: " ++ httpErrorToString err

                                        Ok users ->
                                            OpenScreen <| ScreenImpression ImpressionLike users
                                )
                            )

                        NavButtonMyProfile ->
                            ( ScreenMyProfile
                            , Cmd.none
                            )
            in
            ( { model | currentScreen = newScreen, lastClickedNavButton = button }
            , cmds
            )

        OpenScreen screen ->
            ( { model | currentScreen = screen }
            , Cmd.none
            )

        OpenScreenImpression impression ->
            let
                ( newScreen, cmds ) =
                    ( ScreenImpression impression []
                    , Backend.getUserImpressionsByImpression model.settings.backendUrl
                        impression
                        (\errorOrUsers ->
                            case errorOrUsers of
                                Err err ->
                                    GotUnrecoverableErrror <| "Failed getting users for impression " ++ encode 0 (jsonEncImpression impression) ++ ": " ++ httpErrorToString err

                                Ok users ->
                                    OpenScreen <| ScreenImpression impression users
                        )
                    )
            in
            ( { model | currentScreen = newScreen }
            , cmds
            )

        ViewUser info ->
            let
                handleExtInfoLoaded errOrExtInfo =
                    case errOrExtInfo of
                        Err error ->
                            GotUnrecoverableErrror <| "GotUserExamples returned an error:\n" ++ httpErrorToString error

                        Ok extInfo ->
                            OpenScreen <| ScreenOtherUser info extInfo
            in
            ( model
            , Backend.getUserByUserIDProfile model.settings.backendUrl info.userId handleExtInfoLoaded
            )

        SetUserImpression userID impression ->
            let
                updateImpression userWithImpression =
                    if userWithImpression.user.userId == userID then
                        { userWithImpression | impression = Just impression }

                    else
                        userWithImpression

                newScreen =
                    case model.currentScreen of
                        ScreenSearch users ->
                            ScreenSearch <| List.map updateImpression users

                        _ ->
                            model.currentScreen
            in
            ( { model | currentScreen = newScreen }
            , Cmd.batch
                [ Backend.postUserByImpressionByOtherUserID model.settings.backendUrl impression userID GotSetUserImpressionResult
                , swiperSlideNext <| Just "search-swiper"
                ]
            )

        GotSetUserImpressionResult (Ok ()) ->
            ( model
            , Cmd.none
            )

        GotSetUserImpressionResult (Err error) ->
            ( { model | unrecoverableError = Just <| "Liking a user failed:\n" ++ httpErrorToString error }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus code ->
            "BadStatus: " ++ String.fromInt code

        Http.BadBody body ->
            "BadBody: " ++ body


view : Model -> Html Msg
view model =
    case model.unrecoverableError of
        Just error ->
            div [] [ text error ]

        Nothing ->
            div [ class "root" ]
                [ text "header placeholder"
                , case model.currentScreen of
                    ScreenSearch foundUsers ->
                        viewSearch foundUsers

                    ScreenImpression _ users ->
                        viewScreenImpression users

                    ScreenMyProfile ->
                        div [ class "center-content fill-screen" ] <|
                            text "Your profile will come here. But for now there's just this palceholder and the attributions."
                                :: h1 [] [ text "We use:" ]
                                :: List.map (\( label, url ) -> a [ href url ] [ text label ])
                                    [ ( "loading.io", "https://loading.io/" )
                                    , ( "Font Awesome", "https://fontawesome.com/" )
                                    ]

                    ScreenOtherUser userInfo extInfo ->
                        User.viewProfile SetUserImpression userInfo extInfo
                , case model.currentScreen of
                    ScreenImpression impression _ ->
                        viewScreenImpressionNavbar impression

                    _ ->
                        text ""
                , let
                    icon navButton =
                        case navButton of
                            NavButtonSearch ->
                                "fa-solid fa-magnifying-glass"

                            NavButtonImpressions ->
                                "fa-solid fa-heart-pulse"

                            NavButtonMyProfile ->
                                "fa-solid fa-user"
                  in
                  navbar <|
                    List.map
                        (\navButton ->
                            { icon = icon navButton
                            , onSelect = ClickedNavButton navButton
                            , isSelected = navButton == model.lastClickedNavButton
                            }
                        )
                        allNavButtons
                ]


viewSearch : List UserWithImpression -> Html Msg
viewSearch users =
    let
        viewSlide { user, impression } =
            Swiper.slide
                [ Html.img
                    [ src user.userHeaderImageUrl
                    ]
                    []
                , div [ class "overlay" ]
                    [ div [ class "name" ]
                        [ text <| user.userName
                        ]
                    , let
                        infoRow icon t =
                            tr []
                                [ td [] [ i [ class icon ] [] ]
                                , td [] [ text t ]
                                ]
                      in
                      table [ class "info" ]
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
                                , onClick <| SetUserImpression user.userId impr
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
    in
    div [ class "search-view" ]
        [ Swiper.container
            [ Swiper.id "search-swiper"
            , Swiper.effect Swiper.EffectCards
            ]
            (List.map viewSlide users)
        ]


viewUserCard : UserOverviewInfo -> Html Msg
viewUserCard userInfo =
    User.viewCard [ onClick <| ViewUser userInfo ] userInfo


viewScreenImpression : List UserOverviewInfo -> Html Msg
viewScreenImpression users =
    div []
        [ div
            [ class "masonry" ]
            (List.map
                viewUserCard
                users
            )
        ]


viewScreenImpressionNavbar : Impression -> Html Msg
viewScreenImpressionNavbar impression =
    let
        icon impr =
            case impr of
                ImpressionLike ->
                    "fa-solid fa-heart"

                ImpressionDislike ->
                    "fa-solid fa-heart-broken"

                ImpressionDecideLater ->
                    "fa-solid fa-clock"

                ImpressionSuperLike ->
                    "fa-solid fa-hand-holding-heart"
    in
    navbar <|
        List.map
            (\impr ->
                { icon = icon impr
                , onSelect = OpenScreenImpression impr
                , isSelected = impr == impression
                }
            )
            allImpressions
