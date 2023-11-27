module Main exposing (..)

import Browser
import Either exposing (Either(..))
import Generated.Backend as Backend exposing (User, UserID(..))
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Http
import Store exposing (Requested, Store, unRequest)
import User
import User.Loading


type MainScreen
    = ScreenMatches
    | ScreenSettings
    | Attributions


allMainScreens : List MainScreen
allMainScreens =
    [ ScreenMatches, ScreenSettings, Attributions ]


type SubScreen
    = ScreenUser (Requested UserID)


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
        { userStore : Store UserID User
        , currentScreen : Screen
        , requestedUsers : List (Requested UserID)
        }
    | UnrecoverableError String


type Msg
    = OpenScreen Screen
    | ViewUser UserID
    | GotUserExamples (Result Http.Error (List UserID))
    | AddUserToStore (Result Http.Error (List User))


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
    let
        store : Store UserID User
        store =
            Store.init (\(UserID { unUserID }) -> String.fromInt unUserID) .userID
    in
    ( AllGood
        { userStore = store
        , currentScreen = Main ScreenMatches
        , requestedUsers = []
        }
    , Backend.getUserExampleIDs baseUrl GotUserExamples
    )


requestUsers : Store UserID User -> List UserID -> ( List (Requested UserID), Cmd Msg )
requestUsers store =
    let
        getMissing : List UserID -> Cmd Msg
        getMissing ids =
            Backend.postUserGetMany baseUrl ids AddUserToStore
    in
    Store.mkRequestCommand getMissing store


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

                ViewUser userID ->
                    let
                        ( requestedUsers, storeCmd ) =
                            requestUsers model.userStore [ userID ]
                    in
                    case requestedUsers of
                        [ requestedUserID ] ->
                            ( AllGood { model | currentScreen = Sub (getMain model.currentScreen) (ScreenUser requestedUserID) }
                            , storeCmd
                            )

                        [] ->
                            ( UnrecoverableError <| "We requested one user but got none back. This should never happen."
                            , Cmd.none
                            )

                        _ ->
                            ( UnrecoverableError <| "We requested one user but got multiple back. This should never happen."
                            , Cmd.none
                            )

                GotUserExamples (Ok exampleIDs) ->
                    let
                        ( requestedUsers, cmd ) =
                            requestUsers model.userStore exampleIDs
                    in
                    ( AllGood { model | requestedUsers = requestedUsers }
                    , cmd
                    )

                GotUserExamples (Err error) ->
                    ( UnrecoverableError <| "GotUserExamples returned an error:\n" ++ httpErrorToString error, Cmd.none )

                AddUserToStore (Ok newUsers) ->
                    ( AllGood { model | userStore = Store.update model.userStore newUsers }
                    , Cmd.none
                    )

                AddUserToStore (Err error) ->
                    ( UnrecoverableError <| "AddUserToStore returned an error:\n" ++ httpErrorToString error, Cmd.none )


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
                            viewUserOrID : Either (Requested UserID) User -> Html Msg
                            viewUserOrID userOrID =
                                case userOrID of
                                    Right u ->
                                        User.viewCard [ onClick <| ViewUser u.userID ] u

                                    Left uID ->
                                        User.Loading.viewCardLoading [ onClick <| ViewUser (unRequest uID) ] uID
                        in
                        div []
                            [ div
                                [ class "masonry" ]
                                (List.map
                                    viewUserOrID
                                    (Store.getEithers model.userStore model.requestedUsers)
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

                    Sub _ (ScreenUser userID) ->
                        case Store.getOne model.userStore userID of
                            Just u ->
                                User.viewProfile u

                            Nothing ->
                                div [ class "center-content fill-screen" ] [ text "Loading User..." ]
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
