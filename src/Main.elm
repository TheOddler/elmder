module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html.Keyed
import List.Extra as List
import Random
import Swipe
import User exposing (User)
import User.Random as User


type alias Model =
    { knownUsers : List User
    , swipeInternalState : Swipe.InternalState
    , testOffset : { x : Float, y : Float }
    }


type Msg
    = GetUsers (List User)
    | SwipeInternalMsg Swipe.InternalMsg


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
      , swipeInternalState = Swipe.init
      , testOffset = { x = 0, y = 0 }
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

        SwipeInternalMsg msg ->
            let
                ( newModel, event, cmd ) =
                    Swipe.internalUpdate msg model.swipeInternalState

                newOffset =
                    case event of
                        Nothing ->
                            model.testOffset

                        Just (Swipe.Start _) ->
                            { x = 0, y = 0 }

                        Just (Swipe.Move startPos curPos) ->
                            { x = curPos.x - startPos.x, y = curPos.y - startPos.y }

                        Just (Swipe.End _ _) ->
                            { x = 0, y = 0 }
            in
            ( { model | swipeInternalState = newModel, testOffset = newOffset }
            , Cmd.map SwipeInternalMsg cmd
            )


view : Model -> Html Msg
view model =
    div
        [ class "container is-max-desktop" ]
        [ div [ class "columns" ]
            [ div [ class "column" ] <|
                List.map User.viewCard model.knownUsers
            ]
        ]


keyedDiv : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
keyedDiv =
    Html.Keyed.node "div"
