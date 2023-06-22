module Feed exposing (..)

import Either exposing (Either(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import User exposing (UserID)
import User.Store exposing (RequestedUserID, UserStore, unRequestUserID)


type alias Model =
    { userIDs : List RequestedUserID
    }


type Msg
    = AddUsers (List UserID)


init : (List UserID -> ( List RequestedUserID, Cmd msg )) -> ( Model, Cmd msg )
init requestUsers =
    let
        -- For new just dome dummy data
        initialIDs =
            [ "1", "1", "2", "3", "4", "5", "6" ]

        ( requestedIds, requestCmd ) =
            requestUsers initialIDs
    in
    ( { userIDs = requestedIds
      }
    , requestCmd
    )


update : (List UserID -> ( List RequestedUserID, Cmd msg )) -> Msg -> Model -> ( Model, Cmd msg )
update requestUsers message model =
    case message of
        AddUsers newIDs ->
            let
                allUserIDs =
                    List.concat [ List.map unRequestUserID model.userIDs, newIDs ]

                ( requestedIds, requestCmd ) =
                    requestUsers allUserIDs
            in
            ( { model | userIDs = requestedIds }
            , requestCmd
            )


view : UserStore -> (Msg -> msg) -> (UserID -> msg) -> Model -> Html msg
view userStore msgWrapper viewUser model =
    let
        viewUserOrID userOrID =
            case userOrID of
                Right u ->
                    User.viewCard [ onClick <| viewUser u.id ] u

                Left uID ->
                    User.viewCardLoading [ onClick <| viewUser uID ]
    in
    div []
        [ button
            [ onClick <| msgWrapper <| AddUsers [ "2", "7", "8", "9" ] ]
            [ text "Add users" ]
        , div
            [ class "masonry" ]
            (List.map
                viewUserOrID
                (User.Store.getUsersOrID userStore model.userIDs)
            )
        ]
