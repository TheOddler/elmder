module Main exposing (..)

import Browser
import Either exposing (Either(..))
import Generated.Backend as Backend exposing (Impression(..), UserExtendedInfo, UserID, UserOverviewInfo, jsonEncImpression)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Http
import Json.Encode exposing (encode)
import Ports exposing (swiperSlideNext)
import StringExtra as String
import Swiper
import User exposing (UserInteractions, UserWithImpression)


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


type Screen
    = ScreenSearch (List UserWithImpression)
    | ScreenImpression Impression (List UserOverviewInfo)
    | ScreenMyProfile
    | ScreenOtherUser UserOverviewInfo UserExtendedInfo


type alias Model =
    { settings : AppSettings
    , currentScreen : Screen
    , lastClickedNavButton : NavButton
    , errorMessages : List String
    }


type Msg
    = ClickedNavButton NavButton
    | OpenScreen Screen
    | OpenScreenImpression Impression
    | ViewUser UserOverviewInfo
    | AddErrorMessage String
    | ClearErrorMessages
    | SetUserImpression UserID Impression
    | GotSetUserImpressionResult UserID (Result Http.Error ())


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
      , errorMessages = []
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
                    AddErrorMessage <| "Failed searching for users: " ++ httpErrorToString err

                Ok users ->
                    OpenScreen <|
                        ScreenSearch <|
                            List.map (\u -> { user = u, impression = Nothing })
                                users
        )


updateImpression : UserID -> Maybe Impression -> Model -> Model
updateImpression userID impression model =
    let
        updateUser userWithImpression =
            if userWithImpression.user.userId == userID then
                { userWithImpression | impression = impression }

            else
                userWithImpression

        newScreen =
            case model.currentScreen of
                ScreenSearch users ->
                    ScreenSearch <| List.map updateUser users

                _ ->
                    model.currentScreen
    in
    { model | currentScreen = newScreen }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AddErrorMessage error ->
            ( { model | errorMessages = error :: model.errorMessages }, Cmd.none )

        ClearErrorMessages ->
            ( { model | errorMessages = [] }, Cmd.none )

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
                                            AddErrorMessage <| "Failed getting liked users: " ++ httpErrorToString err

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
                                    AddErrorMessage <| "Failed getting users for impression " ++ encode 0 (jsonEncImpression impression) ++ ": " ++ httpErrorToString err

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
                            AddErrorMessage <| "GotUserExamples returned an error:\n" ++ httpErrorToString error

                        Ok extInfo ->
                            OpenScreen <| ScreenOtherUser info extInfo
            in
            ( model
            , Backend.getUserByUserIDProfile model.settings.backendUrl info.userId handleExtInfoLoaded
            )

        SetUserImpression userID impression ->
            ( updateImpression userID (Just impression) model
            , Cmd.batch
                [ Backend.postUserByImpressionByOtherUserID model.settings.backendUrl impression userID (GotSetUserImpressionResult userID)
                , swiperSlideNext <| Just "search-swiper"
                ]
            )

        GotSetUserImpressionResult _ (Ok ()) ->
            ( model
              -- The user was already optimistically updated
            , Cmd.none
            )

        GotSetUserImpressionResult userID (Err error) ->
            let
                ( newModel, nextCmd ) =
                    update (AddErrorMessage <| "Failed registering user impression:\n" ++ httpErrorToString error) model
            in
            ( updateImpression userID Nothing newModel
            , nextCmd
            )


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
    case model.errorMessages of
        [] ->
            div [ class "root" ]
                [ text "header placeholder"
                , case model.currentScreen of
                    ScreenSearch foundUsers ->
                        viewSearch foundUsers

                    ScreenImpression _ users ->
                        viewScreenImpression users

                    ScreenMyProfile ->
                        div [ class "center-content" ] <|
                            text "Your profile will come here. But for now there's just this palceholder and the attributions."
                                :: h1 [] [ text "We use:" ]
                                :: List.map (\( label, url ) -> a [ href url ] [ text label ])
                                    [ ( "loading.io", "https://loading.io/" )
                                    , ( "Font Awesome", "https://fontawesome.com/" )
                                    ]

                    ScreenOtherUser userInfo extInfo ->
                        User.viewProfile userInteractions userInfo extInfo
                , let
                    navIcon navButton =
                        case navButton of
                            NavButtonSearch ->
                                "fa-solid fa-magnifying-glass"

                            NavButtonImpressions ->
                                "fa-solid fa-heart-pulse"

                            NavButtonMyProfile ->
                                "fa-solid fa-user"

                    imprIcon impr =
                        case impr of
                            ImpressionLike ->
                                "fa-solid fa-heart"

                            ImpressionDislike ->
                                "fa-solid fa-heart-broken"

                            ImpressionDecideLater ->
                                "fa-solid fa-clock"

                            ImpressionSuperLike ->
                                "fa-solid fa-hand-holding-heart"

                    mkMainButton navButton =
                        { icon = navIcon navButton
                        , onSelect = ClickedNavButton navButton
                        , isSelected = navButton == model.lastClickedNavButton
                        }

                    searchButton =
                        mkMainButton NavButtonSearch

                    impressionsButtons =
                        case model.currentScreen of
                            ScreenImpression impression _ ->
                                let
                                    mkImpressionButton impr =
                                        { icon = imprIcon impr
                                        , onSelect = OpenScreenImpression impr
                                        , isSelected = impr == impression
                                        }
                                in
                                List.map mkImpressionButton allImpressions

                            _ ->
                                [ mkMainButton NavButtonImpressions ]

                    myProfileButton =
                        mkMainButton NavButtonMyProfile
                  in
                  navbar <|
                    searchButton
                        :: impressionsButtons
                        ++ [ myProfileButton ]
                ]

        errors ->
            div []
                [ h1 [] [ text "We got one or more errors:" ]
                , ul [] <| List.map (\err -> li [] [ text err ]) errors
                , button [ onClick ClearErrorMessages ] [ text "Clear errors" ]
                ]


userInteractions : UserInteractions Msg
userInteractions =
    { setImpression = SetUserImpression
    , viewProfile = ViewUser
    }


viewSearch : List UserWithImpression -> Html Msg
viewSearch users =
    let
        viewSlide userWithImpr =
            User.viewCardAsSwiperSlide
                userInteractions
                userWithImpr
    in
    div [ class "search-view" ]
        [ Swiper.container
            [ Swiper.id "search-swiper"
            , Swiper.effect Swiper.EffectCards
            ]
            (List.map viewSlide users)
        ]


viewScreenImpression : List UserOverviewInfo -> Html Msg
viewScreenImpression users =
    div
        [ class "user-overview scrollable" ]
        (List.map
            (\userInfo ->
                User.viewCard
                    userInteractions
                    [ onClick <| ViewUser userInfo ]
                    { user = userInfo, impression = Nothing }
            )
            users
        )
