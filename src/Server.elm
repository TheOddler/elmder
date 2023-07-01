module Server exposing (..)

-- For now this module just fakes these requests as we don't actually have any server yet

import Random exposing (Generator)
import User exposing (User, UserID)
import User.Random


fakeHttpRequest : Generator a -> Cmd a
fakeHttpRequest =
    Random.generate identity


getUsers : List UserID -> Cmd (List User)
getUsers idsToRequest =
    -- IRL we'd do a backend call here (we should support batch requesting users), but for now just generate random users
    List.map User.Random.forID idsToRequest
        |> List.foldr (Random.map2 (::)) (Random.constant [])
        |> fakeHttpRequest
