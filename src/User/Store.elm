module User.Store exposing (Msg, RequestedUserID, UserStore, getMaybeUsers, getUser, getUsers, getUsersOrID, init, mkRequestCommand, unRequestUserID, update)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Maybe.Extra as Maybe
import Random
import User exposing (User, UserID)
import User.Random exposing (forID)


{-| A user id that you've requested to the store to fetch.
With this I encode into the types that a certain user ID will eventually be available in the store.
Well, it could still be that the user with the given id doesn't exist at all, but that should only happen if someone is messing with the code, the backend should never return a non-existent user ID.
-}
type RequestedUserID
    = RequestedUserID UserID


unRequestUserID : RequestedUserID -> UserID
unRequestUserID (RequestedUserID id) =
    id


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


mkRequestCommand : UserStore -> List UserID -> ( List RequestedUserID, Cmd Msg )
mkRequestCommand store wantedUsers =
    let
        missingWantedUsers : List UserID
        missingWantedUsers =
            List.filter (\u -> not <| List.member u <| Dict.keys store) wantedUsers

        fakeGetUsersFromServer : List UserID -> Cmd (List User)
        fakeGetUsersFromServer idsToRequest =
            -- IRL we'd do a backend call here (we should support batch requesting users), but for now just generate random users
            List.map forID idsToRequest
                |> List.foldr (Random.map2 (::)) (Random.constant [])
                |> Random.generate identity

        requestedIDs =
            List.map RequestedUserID wantedUsers
    in
    ( requestedIDs
    , if List.isEmpty missingWantedUsers then
        Cmd.none

      else
        Cmd.map UpdateUsers <| fakeGetUsersFromServer missingWantedUsers
    )


getUser : UserStore -> RequestedUserID -> Maybe User
getUser store (RequestedUserID id) =
    Dict.get id store


{-| Get users from the store.
It returns all the users it already knows about, so you can start rendering the UI.
This Cmd should then be run somehow (probably return it to whatever owns the store), and it'll update the store.
This will then trigger a new render cycle and then you should get all the users requested (assuming nothing else changed).
-}
getUsers : UserStore -> List RequestedUserID -> List User
getUsers store wantedUsers =
    Maybe.values <| getMaybeUsers store wantedUsers


{-| Similar to getUsers, but returns a list of `Maybe Uesr`.
This can be used to start rendering a loading space for the unknown users,
and then when the command finishes and updates the store those users will be loaded.
-}
getMaybeUsers : UserStore -> List RequestedUserID -> List (Maybe User)
getMaybeUsers store wantedUsers =
    List.map (getUser store) wantedUsers


{-| Similar to getMaybeUsers, but for the missing users you get their ID back.
This is just a helper function as in some cases you could show a loading user block that is still clickable if you only need the userID to make it clickable.
-}
getUsersOrID : UserStore -> List RequestedUserID -> List (Either RequestedUserID User)
getUsersOrID store wantedUsers =
    let
        getUserEither userID =
            case getUser store userID of
                Just u ->
                    Right u

                Nothing ->
                    Left userID
    in
    List.map getUserEither wantedUsers
