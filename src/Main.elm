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
    | -- You can specify another Msg that will be called when after the store has been updated,
      -- that will allow you to notify whatever other update function you got the store msg from
      UserStoreMsg Msg User.Store.Msg
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

        ( feedModel, storeCmd ) =
            Feed.init store
    in
    ( { userStore = User.Store.init
      , currentScreen = ScreenFeed feedModel
      }
    , Cmd.map (UserStoreMsg <| FeedMsg Feed.Refresh) storeCmd
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

        UserStoreMsg followupMsg storeMsg ->
            -- Update the store,
            { model | userStore = User.Store.update model.userStore storeMsg }
                -- and then do the followup update
                |> update followupMsg

        FeedMsg msg ->
            case model.currentScreen of
                ScreenFeed feed ->
                    let
                        ( feedModel, storeCmd ) =
                            Feed.update model.userStore msg feed
                    in
                    ( { model | currentScreen = ScreenFeed feedModel }
                    , Cmd.map (UserStoreMsg <| FeedMsg Feed.onStoreFollowup) storeCmd
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
                Feed.view FeedMsg ViewUser feed

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
