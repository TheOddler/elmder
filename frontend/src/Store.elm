module Store exposing (Requested, Store, getEithers, getMany, getMaybes, getOne, init, mkRequestCommand, unRequest, update)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Maybe.Extra as Maybe


type alias Store id value =
    { idToString : id -> String
    , getID : value -> id
    , cache : Dict String value
    }


type Requested id
    = Requested id


unRequest : Requested id -> id
unRequest (Requested id) =
    id


init : (id -> String) -> (value -> id) -> Store id value
init idToString getID =
    { idToString = idToString
    , getID = getID
    , cache = Dict.empty
    }


update : Store id value -> List value -> Store id value
update store newThings =
    { store
        | cache =
            List.foldr (\value -> Dict.insert (store.idToString <| store.getID value) value) store.cache newThings
    }


mkRequestCommand : (List id -> Cmd msg) -> Store id value -> List id -> ( List (Requested id), Cmd msg )
mkRequestCommand getMissing store wantedIDs =
    let
        isInCache : id -> Bool
        isInCache u =
            List.member (store.idToString u) (Dict.keys store.cache)

        missingThings : List id
        missingThings =
            List.filter (not << isInCache) wantedIDs

        requestedIDs =
            List.map Requested wantedIDs
    in
    ( requestedIDs
    , if List.isEmpty missingThings then
        Cmd.none

      else
        getMissing missingThings
    )


getOne : Store id value -> Requested id -> Maybe value
getOne store (Requested id) =
    Dict.get (store.idToString id) store.cache


{-| Get values from the store.
It returns all the values it already knows about, so you can start rendering the UI.
This Cmd should then be run somehow (probably return it to whatever owns the store), and it'll update the store.
This will then trigger value new render cycle and then you should get all the values requested (assuming nothing else changed).
-}
getMany : Store id value -> List (Requested id) -> List value
getMany store wantedIDs =
    Maybe.values <| getMaybes store wantedIDs


{-| Similar to getMany, but returns value list of `Maybe Uesr`.
This can be used to start rendering value loading space for the unknown values,
and then when the command finishes and updates the store those values will be loaded.
-}
getMaybes : Store id value -> List (Requested id) -> List (Maybe value)
getMaybes store wantedIDs =
    List.map (getOne store) wantedIDs


{-| Similar to getMaybes, but for the missing values you get their ID back.
This is just value helper function as in some cases you could show value loading value block that is still clickable if you only need the ID to make it clickable.
-}
getEithers : Store id value -> List (Requested id) -> List (Either (Requested id) value)
getEithers store wantedIDs =
    let
        getEither id =
            case getOne store id of
                Just u ->
                    Right u

                Nothing ->
                    Left id
    in
    List.map getEither wantedIDs
