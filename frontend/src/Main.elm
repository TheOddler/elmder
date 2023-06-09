module Main exposing (..)

import Browser
import Either exposing (Either(..))
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Server exposing (User, UserID)
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


type alias Model =
    { userStore : Store UserID User
    , currentScreen : Screen
    , requestedUsers : List (Requested UserID)
    }


type Msg
    = OpenScreen Screen
    | ViewUser UserID
    | AddUserToStore (List User)


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


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        store =
            Store.init Server.getUsers .id

        mockUserIds =
            [ "1"
            , "2"
            , "3"
            , "4"
            , "5"
            , "6"
            , "7"
            , "8"
            , "9"
            , "10"
            ]

        ( requestedMockUsers, storeCmd ) =
            Store.mkRequestCommand store mockUserIds
    in
    ( { userStore = store
      , currentScreen = Main ScreenMatches
      , requestedUsers = requestedMockUsers
      }
    , Cmd.map AddUserToStore storeCmd
    )


requestUsers : Store UserID User -> List UserID -> ( List (Requested UserID), Cmd Msg )
requestUsers store =
    Tuple.mapSecond (Cmd.map AddUserToStore) << Store.mkRequestCommand store


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OpenScreen screen ->
            ( { model | currentScreen = screen }
            , Cmd.none
            )

        ViewUser userID ->
            let
                ( requestedUsers, storeCmd ) =
                    requestUsers model.userStore [ userID ]
            in
            case requestedUsers of
                [ requestedUserID ] ->
                    ( { model | currentScreen = Sub (getMain model.currentScreen) (ScreenUser requestedUserID) }
                    , storeCmd
                    )

                _ ->
                    -- This should never happen, we request one userID so we should get one back.
                    ( model
                    , storeCmd
                    )

        AddUserToStore newUsers ->
            ( { model | userStore = Store.update model.userStore newUsers }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ case model.currentScreen of
            Main ScreenMatches ->
                let
                    viewUserOrID userOrID =
                        case userOrID of
                            Right u ->
                                User.viewCard [ onClick <| ViewUser u.id ] u

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
