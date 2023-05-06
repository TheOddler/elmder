module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div, img, text)
import Html.Attributes as Attributes exposing (class, src)
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
    , swipeCurrentPosition : Maybe Swipe.Position
    }


type Msg
    = SaveForLater User
    | SwipeInternalMsg Swipe.InternalMsg
    | SetSwipePos Swipe.Position
    | EndSwipe


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
      , swipeCurrentPosition = Nothing
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

                newPos =
                    case event of
                        Nothing ->
                            model.swipeCurrentPosition

                        Just (Swipe.Start startPos) ->
                            Just startPos

                        Just (Swipe.Move _ curPos) ->
                            Just curPos

                        Just (Swipe.End _ _) ->
                            Nothing
            in
            ( { model | swipeInternalState = newModel, swipeCurrentPosition = newPos }
            , Cmd.map SwipeInternalMsg cmd
            )

        SetSwipePos newPos ->
            ( { model | swipeCurrentPosition = Just newPos }
            , Cmd.none
            )

        EndSwipe ->
            ( { model | swipeCurrentPosition = Nothing }
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
            (class "swipe_tryout"
                :: Swipe.onSwipe model.swipeInternalState SwipeInternalMsg
            )
            [ text <|
                case model.swipeCurrentPosition of
                    Nothing ->
                        ""

                    Just pos ->
                        String.fromFloat pos.x ++ "," ++ String.fromFloat pos.y
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
