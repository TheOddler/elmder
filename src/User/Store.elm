module User.Store exposing (Msg, UserStore, getMaybeUsers, getUsers, init, update)

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Random
import User exposing (User, UserID)
import User.Random exposing (forID)


type alias UserStore =
    Dict UserID User


init : UserStore
init =
    Dict.empty


type Msg
    = UpdateUsers (List User)


update : UserStore -> Msg -> UserStore
update store msg =
    case msg of
        UpdateUsers newUsers ->
            List.foldr (\u -> Dict.insert u.id u) store newUsers


{-| Get users from the store.
It returns all the users it already knows about, so you can start rendering the UI.
This Cmd should then be run somehow (probably return it to whatever owns the store), and it'll update the store.
This will then trigger a new render cycle and then you should get all the users requested (assuming nothing else changed).
-}
getUsers : UserStore -> List UserID -> ( List User, Cmd Msg )
getUsers store wantedUsers =
    getMaybeUsers store wantedUsers
        |> Tuple.mapFirst Maybe.values


{-| Similar to getUsers, but returns a list of `Maybe Uesr`.
This can be used to start rendering a loading space for the unknown users,
and then when the command finishes and updates the store those users will be loaded.
-}
getMaybeUsers : UserStore -> List UserID -> ( List (Maybe User), Cmd Msg )
getMaybeUsers store wantedUsers =
    let
        maybeUsers =
            List.map (\i -> Dict.get i store) wantedUsers

        userIdsToRequest : List UserID
        userIdsToRequest =
            List.filter (\u -> not <| List.member u <| Dict.keys store) wantedUsers

        fakeGetUsersFromServer : Cmd (List User)
        fakeGetUsersFromServer =
            -- IRL we'd do a backend call here (we should support batch requesting users), but for now just generate random users
            List.map forID userIdsToRequest
                |> List.foldr (Random.map2 (::)) (Random.constant [])
                |> Random.generate identity
    in
    ( maybeUsers
    , if List.any Maybe.isNothing maybeUsers then
        Cmd.map UpdateUsers fakeGetUsersFromServer

      else
        Cmd.none
    )
