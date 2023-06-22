module Main exposing (..)

import Browser
import Feed
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Components exposing (navbar)
import User exposing (User)
import User.Random as User
import User.Store exposing (UserStore)


type Screen
    = ScreenFeed Feed.Model
    | ScreenMatches
    | ScreenSettings
    | ScreenUser User


type alias Model =
    { userStore : UserStore
    , currentScreen : Screen
    }


type Msg
    = OpenScreen Screen
    | ViewUser User
    | UserStoreMsg User.Store.Msg
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
            User.Store.init

        ( feedModel, feedMsg ) =
            Feed.init (Cmd.map UserStoreMsg << User.Store.mkUpdateCommand store)
    in
    ( { userStore = store
      , currentScreen = ScreenFeed feedModel
      }
    , feedMsg
    )


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

        ViewUser user ->
            ( { model | currentScreen = ScreenUser user }
            , Cmd.none
            )

        UserStoreMsg msg ->
            ( { model | userStore = User.Store.update model.userStore msg }
            , Cmd.none
            )

        FeedMsg msg ->
            case model.currentScreen of
                ScreenFeed feed ->
                    let
                        ( feedModel, nextMsg ) =
                            Feed.update (Cmd.map UserStoreMsg << User.Store.mkUpdateCommand model.userStore) msg feed
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

            ScreenUser user ->
                User.viewProfile user
        , navbar
            [ { onSelect = OpenScreen ScreenMatches
              , icon = "fa-solid fa-heart"
              , isSelected = model.currentScreen == ScreenMatches
              }
            , { onSelect = OpenScreen ScreenSettings
              , icon = "fa-solid fa-gear"
              , isSelected = model.currentScreen == ScreenSettings
              }
            ]
        ]
