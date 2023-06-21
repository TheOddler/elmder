module Feed exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import User exposing (User, UserID)
import User.Store exposing (UserStore)


type alias Model =
    { userIDs : List UserID
    , usersCache : List User
    }


type Msg
    = Refresh
    | AddUsers (List UserID)


init : UserStore -> ( Model, Cmd User.Store.Msg )
init userStore =
    let
        pretendUserIDs =
            [ "1", "1", "2", "3", "4", "5", "6" ]

        ( users, storeCmd ) =
            User.Store.getUsers userStore pretendUserIDs
    in
    ( { userIDs = pretendUserIDs
      , usersCache = users
      }
    , storeCmd
    )


onStoreFollowup : Msg
onStoreFollowup =
    Refresh


update : UserStore -> Msg -> Model -> ( Model, Cmd User.Store.Msg )
update userStore message model =
    case message of
        Refresh ->
            let
                ( users, storeCmd ) =
                    User.Store.getUsers userStore model.userIDs
            in
            ( { model | usersCache = users }
            , storeCmd
            )

        AddUsers newIDs ->
            let
                newUserIDs =
                    List.concat [ model.userIDs, newIDs ]

                ( users, storeCmd ) =
                    User.Store.getUsers userStore newUserIDs
            in
            ( { model
                | userIDs = newUserIDs
                , usersCache = users
              }
            , storeCmd
            )


view : (Msg -> msg) -> (User -> msg) -> Model -> Html msg
view msgWrapper onClickUser model =
    div []
        [ button
            [ onClick <| msgWrapper <| AddUsers [ "2", "7", "8", "9" ] ]
            [ text "Add users" ]
        , div
            [ class "masonry" ]
            (List.map (\u -> User.viewCard [ onClick <| onClickUser u ] u) model.usersCache)
        ]
