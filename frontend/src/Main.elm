module Main exposing (..)

import Browser
import Either exposing (Either(..))
import Generated.Backend as Backend exposing (UserOverviewInfo)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Http
import User


type MainScreen
    = ScreenMatches
    | ScreenSettings
    | Attributions


allMainScreens : List MainScreen
allMainScreens =
    [ ScreenMatches, ScreenSettings, Attributions ]


type SubScreen
    = ScreenUser UserOverviewInfo


type Screen
    = Main MainScreen
    | Sub MainScreen SubScreen


getMain : Screen -> MainScreen
getMain screen =
    case screen of
        Main m ->
            m

        Sub m _ ->
            m


type Model
    = AllGood
        { overviewUsers : List UserOverviewInfo
        , currentScreen : Screen
        }
    | UnrecoverableError String


type Msg
    = OpenScreen Screen
    | ViewUser UserOverviewInfo
    | GotUsersForOverview (Result Http.Error (List UserOverviewInfo))


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
    ( AllGood
        { currentScreen = Main ScreenMatches
        , overviewUsers = []
        }
    , Backend.getUserSearch baseUrl GotUsersForOverview
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update message modelOrError =
    case modelOrError of
        UnrecoverableError _ ->
            ( modelOrError, Cmd.none )

        AllGood model ->
            case message of
                OpenScreen screen ->
                    ( AllGood { model | currentScreen = screen }
                    , Cmd.none
                    )

                ViewUser info ->
                    ( AllGood { model | currentScreen = Sub (getMain model.currentScreen) (ScreenUser info) }
                    , Cmd.none
                      -- TODO: Request more user info here
                    )

                GotUsersForOverview (Ok userInfos) ->
                    ( AllGood { model | overviewUsers = userInfos }
                    , Cmd.none
                    )

                GotUsersForOverview (Err error) ->
                    ( UnrecoverableError <| "GotUserExamples returned an error:\n" ++ httpErrorToString error, Cmd.none )


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
view modelOrError =
    case modelOrError of
        UnrecoverableError error ->
            div [] [ text error ]

        AllGood model ->
            div [ class "root" ]
                [ case model.currentScreen of
                    Main ScreenMatches ->
                        let
                            viewUser : UserOverviewInfo -> Html Msg
                            viewUser userInfo =
                                User.viewCard [ onClick <| ViewUser userInfo ] userInfo
                        in
                        div []
                            [ div
                                [ class "masonry" ]
                                (List.map
                                    viewUser
                                    model.overviewUsers
                                )
                            ]

                    Main ScreenSettings ->
                        div [ class "center-content fill-screen" ] [ text "placeholder for settings screen" ]

                    Main Attributions ->
                        div [ class "center-content fill-screen" ] <|
                            h1 [] [ text "We use:" ]
                                :: List.map (\( label, url ) -> a [ href url ] [ text label ])
                                    [ ( "loading.io", "https://loading.io/" )
                                    , ( "Font Awesome", "https://fontawesome.com/" )
                                    ]

                    Sub _ (ScreenUser userInfo) ->
                        User.viewProfile userInfo
                , let
                    icon mainScreen =
                        case mainScreen of
                            ScreenMatches ->
                                "fa-solid fa-heart"

                            ScreenSettings ->
                                "fa-solid fa-gear"

                            Attributions ->
                                "fa-solid fa-hippo"
                  in
                  navbar <|
                    List.map
                        (\ms ->
                            { icon = icon ms
                            , onSelect = OpenScreen (Main ms)
                            , isSelected = ms == getMain model.currentScreen
                            }
                        )
                        allMainScreens
                ]
