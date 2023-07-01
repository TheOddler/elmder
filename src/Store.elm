module Store exposing (Requested, Store, getEithers, getMany, getMaybes, getOne, init, mkRequestCommand, unRequest, update)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Maybe.Extra as Maybe


type alias Store comparable a =
    { get : List comparable -> Cmd (List a)
    , getKey : a -> comparable
    , cache : Dict comparable a
    }


type Requested id
    = Requested id


unRequest : Requested id -> id
unRequest (Requested id) =
    id


init : (List comparable -> Cmd (List a)) -> (a -> comparable) -> Store comparable a
init get getKey =
    { get = get
    , getKey = getKey
    , cache = Dict.empty
    }


update : Store comparable a -> List a -> Store comparable a
update store newThings =
    { store
        | cache =
            List.foldr (\a -> Dict.insert (store.getKey a) a) store.cache newThings
    }


mkRequestCommand : Store comparable a -> List comparable -> ( List (Requested comparable), Cmd (List a) )
mkRequestCommand store wantedUsers =
    let
        missingThings : List comparable
        missingThings =
            List.filter (\u -> not <| List.member u <| Dict.keys store.cache) wantedUsers

        requestedIDs =
            List.map Requested wantedUsers
    in
    ( requestedIDs
    , if List.isEmpty missingThings then
        Cmd.none

      else
        store.get missingThings
    )


getOne : Store comparable a -> Requested comparable -> Maybe a
getOne store (Requested id) =
    Dict.get id store.cache


{-| Get users from the store.
It returns all the users it already knows about, so you can start rendering the UI.
This Cmd should then be run somehow (probably return it to whatever owns the store), and it'll update the store.
This will then trigger a new render cycle and then you should get all the users requested (assuming nothing else changed).
-}
getMany : Store comparable a -> List (Requested comparable) -> List a
getMany store wantedUsers =
    Maybe.values <| getMaybes store wantedUsers


{-| Similar to getUsers, but returns a list of `Maybe Uesr`.
This can be used to start rendering a loading space for the unknown users,
and then when the command finishes and updates the store those users will be loaded.
-}
getMaybes : Store comparable a -> List (Requested comparable) -> List (Maybe a)
getMaybes store wantedUsers =
    List.map (getOne store) wantedUsers


{-| Similar to getMaybeUsers, but for the missing users you get their ID back.
This is just a helper function as in some cases you could show a loading user block that is still clickable if you only need the userID to make it clickable.
-}
getEithers : Store comparable a -> List (Requested comparable) -> List (Either (Requested comparable) a)
getEithers store wantedUsers =
    let
        getUserEither userID =
            case getOne store userID of
                Just u ->
                    Right u

                Nothing ->
                    Left userID
    in
    List.map getUserEither wantedUsers
