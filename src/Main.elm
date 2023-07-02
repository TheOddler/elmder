module Main exposing (..)

import Browser
import Feed
import Html exposing (Html, a, div, h1, text, ul)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Server exposing (Feed, FeedID, User, UserID)
import Store exposing (Requested, Store)
import User


type MainScreen
    = ScreenFeeds
    | ScreenMatches
    | ScreenSettings
    | Attributions


allMainScreens : List MainScreen
allMainScreens =
    [ ScreenFeeds, ScreenMatches, ScreenSettings, Attributions ]


type SubScreen
    = ScreenUser (Requested UserID)
    | ScreenFeed Feed.Model


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
    , feeds : List Feed
    }


type Msg
    = OpenScreen Screen
    | ViewUser UserID
    | AddUserToStore (List User)
    | FeedsLoaded (List Feed)
    | FeedMsg FeedID Feed.Msg


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
    in
    ( { userStore = store
      , currentScreen = Main ScreenFeeds
      , feeds = []
      }
    , Cmd.map FeedsLoaded Server.getFeeds
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

        FeedMsg feedID msg ->
            Debug.todo "Needs implementing"

        FeedsLoaded feeds ->
            ( { model | feeds = feeds }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ case model.currentScreen of
            Main ScreenFeeds ->
                ul [] <|
                    List.map Feed.viewListCard model.feeds

            Main ScreenMatches ->
                div [ class "center-content fill-screen" ] [ text "placeholder for matches screen" ]

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

            Sub _ (ScreenFeed feed) ->
                Debug.todo "To Implement"
        , let
            icon mainScreen =
                case mainScreen of
                    ScreenFeeds ->
                        "fa-solid fa-magnifying-glass"

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
