module Main exposing (..)

import Browser
import Either exposing (Either(..))
import Generated.Backend as Backend exposing (UserExtendedInfo, UserID, UserOverviewInfo)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Http
import User


type NavButton
    = NavButtonSearch
    | NavButtonLikes
    | NavButtonMyProfile


allNavButtons : List NavButton
allNavButtons =
    [ NavButtonSearch
    , NavButtonLikes
    , NavButtonMyProfile
    ]


type Screen
    = ScreenSearch (List UserOverviewInfo)
    | ScreenLikes (List UserOverviewInfo)
    | ScreenMyProfile
    | ScreenOtherUser UserOverviewInfo UserExtendedInfo


type alias Model =
    { currentScreen : Screen
    , lastClickedNavButton : NavButton
    , unrecoverableError : Maybe String
    }


type Msg
    = ClickedNavButton NavButton
    | OpenScreen Screen
    | ViewUser UserOverviewInfo
    | GotUnrecoverableErrror String
    | LikeUser UserID
    | LikedUserResult (Result Http.Error ())


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


baseUrl : String
baseUrl =
    "http://localhost:8081"


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { currentScreen = ScreenSearch []
      , lastClickedNavButton = NavButtonSearch
      , unrecoverableError = Nothing
      }
    , openScreenSearch
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


openScreenSearch =
    Backend.getUserSearch baseUrl
        (\errorOrUsers ->
            case errorOrUsers of
                Err err ->
                    GotUnrecoverableErrror <| httpErrorToString err

                Ok users ->
                    OpenScreen <| ScreenSearch users
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
                            , openScreenSearch
                            )

                        NavButtonLikes ->
                            ( ScreenLikes []
                            , Backend.getUserLikes baseUrl
                                (\errorOrUsers ->
                                    case errorOrUsers of
                                        Err err ->
                                            GotUnrecoverableErrror <| httpErrorToString err

                                        Ok users ->
                                            OpenScreen <| ScreenLikes users
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
            , Backend.getUserByUserIDProfile baseUrl info.userId handleExtInfoLoaded
            )

        LikeUser userID ->
            ( model
            , Backend.postUserByLikedUserIDLike baseUrl userID LikedUserResult
            )

        LikedUserResult (Ok ()) ->
            ( model
            , Cmd.none
            )

        LikedUserResult (Err error) ->
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
                [ let
                    viewUserCard : UserOverviewInfo -> Html Msg
                    viewUserCard userInfo =
                        User.viewCard [ onClick <| ViewUser userInfo ] userInfo
                  in
                  case model.currentScreen of
                    ScreenSearch foundUsers ->
                        div []
                            [ div
                                [ class "masonry" ]
                                (List.map
                                    viewUserCard
                                    foundUsers
                                )
                            ]

                    ScreenLikes likedUsers ->
                        div []
                            [ div
                                [ class "masonry" ]
                                (List.map
                                    viewUserCard
                                    likedUsers
                                )
                            ]

                    ScreenMyProfile ->
                        div [ class "center-content fill-screen" ] <|
                            text "Your profile will come here. But for now there's just this palceholder and the attributions."
                                :: h1 [] [ text "We use:" ]
                                :: List.map (\( label, url ) -> a [ href url ] [ text label ])
                                    [ ( "loading.io", "https://loading.io/" )
                                    , ( "Font Awesome", "https://fontawesome.com/" )
                                    ]

                    ScreenOtherUser userInfo extInfo ->
                        User.viewProfile LikeUser userInfo extInfo
                , let
                    icon navButton =
                        case navButton of
                            NavButtonSearch ->
                                "fa-solid fa-magnifying-glass"

                            NavButtonLikes ->
                                "fa-solid fa-heart"

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
