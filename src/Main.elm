module Main exposing (..)

import Browser
import Feed
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import User exposing (UserID)
import User.Random as User
import User.Store exposing (RequestedUserID, UserStore)


type Screen
    = ScreenFeed Feed.Model
    | ScreenMatches
    | ScreenSettings
    | ScreenUser RequestedUserID
    | Attributions


type alias Model =
    { userStore : UserStore
    , currentScreen : Screen
    }


type Msg
    = OpenScreen Screen
    | ViewUser UserID
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
            Feed.init <| mkrequestUsersFunc store
    in
    ( { userStore = store
      , currentScreen = ScreenFeed feedModel
      }
    , feedMsg
    )


mkrequestUsersFunc : UserStore -> List UserID -> ( List User.Store.RequestedUserID, Cmd Msg )
mkrequestUsersFunc store =
    Tuple.mapSecond (Cmd.map UserStoreMsg) << User.Store.mkRequestCommand store


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

        UserStoreMsg msg ->
            ( { model | userStore = User.Store.update model.userStore msg }
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
                case User.Store.getUser model.userStore userID of
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
