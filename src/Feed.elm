module Feed exposing (..)

import Either exposing (Either(..))
import Faker
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Server exposing (Feed, User, UserID)
import Store exposing (Requested, Store, unRequest)
import User
import User.Loading


type alias Model =
    { feed : Feed
    , requestedUsers : List (Requested UserID)
    }


type Msg
    = AddUsers (List UserID)


init : Feed -> ( Model, Cmd Msg )
init feed =
    let
        -- For new just dome dummy data
        initialIDs =
            Random.int 0 50
                |> Random.map String.fromInt
                |> Faker.listMinMax 5 15
                |> Random.generate AddUsers
    in
    ( { feed = feed
      , requestedUsers = []
      }
    , initialIDs
    )


update : (List UserID -> ( List (Requested UserID), Cmd msg )) -> Msg -> Model -> ( Model, Cmd msg )
update requestUsers message model =
    case message of
        AddUsers newIDs ->
            let
                allUserIDs =
                    List.concat [ List.map unRequest model.requestedUsers, newIDs ]

                ( requestedIds, requestCmd ) =
                    requestUsers allUserIDs
            in
            ( { model | requestedUsers = requestedIds }
            , requestCmd
            )


view : Store UserID User -> (Msg -> msg) -> (UserID -> msg) -> Model -> Html msg
view userStore msgWrapper viewUser model =
    let
        viewUserOrID userOrID =
            case userOrID of
                Right u ->
                    User.viewCard [ onClick <| viewUser u.id ] u

                Left uID ->
                    User.Loading.viewCardLoading [ onClick <| viewUser (unRequest uID) ] uID
    in
    div []
        [ button
            [ onClick <| msgWrapper <| AddUsers [ "2", "7", "8", "9" ] ]
            [ text "Add users" ]
        , div
            [ class "masonry" ]
            (List.map
                viewUserOrID
                (Store.getEithers userStore model.requestedUsers)
            )
        ]


viewListCard : Feed -> Html msg
viewListCard feed =
    div [] [ text feed.name ]
