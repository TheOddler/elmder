module Main exposing (..)

import Browser
import Feed
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Server exposing (User, UserID)
import Store exposing (Requested, Store)
import User


type Screen
    = ScreenFeed Feed.Model
    | ScreenMatches
    | ScreenSettings
    | ScreenUser (Requested UserID)
    | Attributions


type alias Model =
    { userStore : Store UserID User
    , currentScreen : Screen
    }


type Msg
    = OpenScreen Screen
    | ViewUser UserID
    | AddUserToStore (List User)
    | FeedMsg Feed.Msg


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

        ( feedModel, feedMsg ) =
            Feed.init
    in
    ( { userStore = store
      , currentScreen = ScreenFeed feedModel
      }
    , Cmd.map FeedMsg feedMsg
    )


mkrequestUsersFunc : Store UserID User -> List UserID -> ( List (Requested UserID), Cmd Msg )
mkrequestUsersFunc store =
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
                    mkrequestUsersFunc model.userStore [ userID ]
            in
            case requestedUsers of
                [ requestedUserID ] ->
                    ( { model | currentScreen = ScreenUser requestedUserID }
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

        FeedMsg msg ->
            case model.currentScreen of
                ScreenFeed feed ->
                    let
                        ( feedModel, nextMsg ) =
                            Feed.update (mkrequestUsersFunc model.userStore) msg feed
                    in
                    ( { model | currentScreen = ScreenFeed feedModel }
                    , nextMsg
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ case model.currentScreen of
            ScreenFeed feed ->
                Feed.view model.userStore FeedMsg ViewUser feed

            ScreenMatches ->
                div [ class "center-content fill-screen" ] [ text "placeholder for matches screen" ]

            ScreenSettings ->
                div [ class "center-content fill-screen" ] [ text "placeholder for settings screen" ]

            ScreenUser userID ->
                case Store.getOne model.userStore userID of
                    Just u ->
                        User.viewProfile u

                    Nothing ->
                        div [ class "center-content fill-screen" ] [ text "Loading User..." ]

            Attributions ->
                div [ class "center-content fill-screen" ] <|
                    h1 [] [ text "We use:" ]
                        :: List.map (\( label, url ) -> a [ href url ] [ text label ])
                            [ ( "loading.io", "https://loading.io/" )
                            , ( "Font Awesome", "https://fontawesome.com/" )
                            ]
        , navbar
            [ { onSelect = OpenScreen ScreenMatches
              , icon = "fa-solid fa-heart"
              , isSelected = model.currentScreen == ScreenMatches
              }
            , { onSelect = OpenScreen ScreenSettings
              , icon = "fa-solid fa-gear"
              , isSelected = model.currentScreen == ScreenSettings
              }
            , { onSelect = OpenScreen Attributions
              , icon = "fa-solid fa-hippo"
              , isSelected = model.currentScreen == Attributions
              }
            ]
        ]
