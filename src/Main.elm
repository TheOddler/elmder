module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Html.Keyed
import List.Extra as List
import Swipe


type alias User =
    { id : Int
    , name : String
    , imageUrl : String
    }


type alias Model =
    { overviewUsers : List User
    , savedUsers : List User
    , swipeInternalState : Swipe.InternalState
    , testOffset : { x : Float, y : Float }
    }


type Msg
    = SaveForLater User
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


genUsers : Int -> List User
genUsers seed =
    let
        count =
            10

        dummy =
            List.repeat count ()

        userFromId dummyId () =
            let
                id =
                    seed + dummyId

                stringId =
                    String.fromInt id
            in
            { id = id
            , name = "User " ++ stringId
            , imageUrl = "http://placekitten.com/200/300?" ++ stringId
            }
    in
    List.indexedMap userFromId dummy


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { overviewUsers = genUsers 0
      , savedUsers = []
      , swipeInternalState = Swipe.init
      , testOffset = { x = 0, y = 0 }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SaveForLater user ->
            ( { model | overviewUsers = List.remove user model.overviewUsers, savedUsers = user :: model.savedUsers }
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
        [ class "users" ]
        [ keyedDiv [ class "overview" ] <|
            List.map (viewUser Overview) model.overviewUsers
                ++ List.map (viewUser Saved) model.savedUsers
        , div
            ([ class "swipe_tryout"
             , style "transform" <| "translate(" ++ String.fromFloat model.testOffset.x ++ "px, " ++ String.fromFloat model.testOffset.y ++ "px)"
             ]
                ++ Swipe.onSwipe model.swipeInternalState SwipeInternalMsg
            )
            [ text <| String.fromFloat model.testOffset.x ++ "," ++ String.fromFloat model.testOffset.y
            ]
        , div []
            [ text <| Debug.toString model.swipeInternalState ]
        ]


type UserLocation
    = Overview
    | Saved


viewUser : UserLocation -> User -> ( String, Html Msg )
viewUser location user =
    ( "user_id_" ++ String.fromInt user.id
    , div
        [ class <|
            case location of
                Overview ->
                    "user overview"

                Saved ->
                    "user saved"
        , onClick <| SaveForLater user
        ]
        [ text user.name
        , img [ src user.imageUrl ] []
        ]
    )


keyedDiv : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
keyedDiv =
    Html.Keyed.node "div"
