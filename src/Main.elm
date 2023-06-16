module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import List.Extra as List
import Random
import User exposing (User)
import User.Random as User


type Screen
    = ScreenFeed
    | ScreenMatches
    | ScreenSettings
    | ScreenUser User


type alias Model =
    { knownUsers : List User
    , currentScreen : Screen
    }


type Msg
    = GetUsers (List User)
    | OpenScreen Screen
    | ViewUser User


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
    ( { knownUsers = []
      , currentScreen = ScreenFeed
      }
    , Random.generate GetUsers <| Random.list 10 User.random
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GetUsers users ->
            ( { model | knownUsers = users }
            , Cmd.none
            )

        OpenScreen screen ->
            ( { model | currentScreen = screen }
            , Cmd.none
            )

        ViewUser user ->
            ( { model | currentScreen = ScreenUser user }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ case model.currentScreen of
            ScreenFeed ->
                div [ class "masonry" ] (List.map (\u -> User.viewCard [ onClick <| ViewUser u ] u) model.knownUsers)

            ScreenMatches ->
                div [ class "center-content fill-screen" ] [ text "placeholder for matches screen" ]

            ScreenSettings ->
                div [ class "center-content fill-screen" ] [ text "placeholder for settings screen" ]

            ScreenUser user ->
                User.viewProfile user
        , navbar
            [ { onSelect = OpenScreen ScreenFeed
              , icon = "fa-solid fa-users"
              , isSelected = model.currentScreen == ScreenFeed
              }
            , { onSelect = OpenScreen ScreenMatches
              , icon = "fa-solid fa-heart"
              , isSelected = model.currentScreen == ScreenMatches
              }
            , { onSelect = OpenScreen ScreenSettings
              , icon = "fa-solid fa-gear"
              , isSelected = model.currentScreen == ScreenSettings
              }
            ]
        ]
