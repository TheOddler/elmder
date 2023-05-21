module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Components exposing (navbar)
import List.Extra as List
import Random
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
    , viewingUser : Maybe User
    }


type Msg
    = GetUsers (List User)
    | OpenScreen Screen
    | ViewUser User
    | CloseUser


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
      , viewingUser = Nothing
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
            ( { model | viewingUser = Just user }
            , Cmd.none
            )

        CloseUser ->
            ( { model | viewingUser = Nothing }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ case model.currentScreen of
            Feed ->
                div [ class "masonry" ] (List.map (User.viewCard ViewUser) model.knownUsers)

            Matches ->
                div [ class "center-content fill-screen" ] [ text "placeholder for matches screen" ]

            Settings ->
                div [ class "center-content fill-screen" ] [ text "placeholder for settings screen" ]
        , navbar
            { buttons = allScreens
            , getIcon = screenIcons
            , onSelect = OpenScreen
            , selected = model.currentScreen
            }
        , case model.viewingUser of
            Nothing ->
                text ""

            Just viewingUser ->
                User.viewProfileOverlay CloseUser viewingUser
        ]
