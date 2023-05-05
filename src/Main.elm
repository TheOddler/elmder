module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Html.Keyed
import List.Extra as List
import Swipe as Swipe exposing (Gesture(..), attributes)


type alias User =
    { id : Int
    , name : String
    , imageUrl : String
    }


type alias Model =
    { overviewUsers : List User
    , savedUsers : List User
    , testGesture : Gesture
    }


type Msg
    = SaveForLater User
    | UpdateTestGesture Gesture


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
      , testGesture = Swipe.init
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

        UpdateTestGesture gesture ->
            ( { model | testGesture = gesture }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ class "users" ]
        [ keyedDiv [ class "overview" ] <|
            List.map (viewUser Overview) model.overviewUsers
                ++ List.map (viewUser Saved) model.savedUsers
        , div
            (class "swipe_tryout" :: attributes model.testGesture UpdateTestGesture)
            [ text <|
                case model.testGesture of
                    None ->
                        ""

                    Start startPos ->
                        "Starting: " ++ String.fromFloat startPos.x ++ "," ++ String.fromFloat startPos.y

                    Moving _ curPos ->
                        "Moving: " ++ String.fromFloat curPos.x ++ "," ++ String.fromFloat curPos.y

                    Ended _ curPos ->
                        "Ended: " ++ String.fromFloat curPos.x ++ "," ++ String.fromFloat curPos.y
            ]
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
