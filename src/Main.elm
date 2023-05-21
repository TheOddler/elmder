module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html.Components exposing (navbar)
import Html.Keyed
import List.Extra as List
import Random
import Swipe
import User exposing (User)
import User.Random as User


type Screen
    = Feed
    | Matches
    | Settings


allScreens : List Screen
allScreens =
    [ Feed, Matches, Settings ]


{-| Return the fontawesome icon name
This should include the style (regular/solid/...) as not all are available for free
-}
screenIcons : Screen -> String
screenIcons screen =
    case screen of
        Feed ->
            "fa-solid fa-users"

        Matches ->
            "fa-solid fa-heart"

        Settings ->
            "fa-solid fa-gear"


type alias Model =
    { knownUsers : List User
    , currentScreen : Screen
    }


type Msg
    = GetUsers (List User)
    | OpenScreen Screen


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
      , currentScreen = Feed
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


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "masonry" ] (List.map User.viewCard model.knownUsers)
        , navbar
            { buttons = allScreens
            , getIcon = screenIcons
            , onSelect = OpenScreen
            , selected = model.currentScreen
            }
        ]
