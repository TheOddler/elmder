module Server exposing (fakeHttpRequest)

-- For now this module just fakes these requests as we don't actually have any server yet
-- The idea is that everything here is what we generate based on what's on the server

import Faker
import Process
import Random exposing (Generator, map, map2, uniform)
import Random.Extra as Random exposing (andMap)
import Task
import Time


fakeHttpRequest : Generator a -> Cmd a
fakeHttpRequest resultGenerator =
    let
        randomToTask generator =
            Time.now
                |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)
    in
    randomToTask (Random.float 1 1)
        |> Task.andThen (\s -> Process.sleep <| s * 1000)
        |> Task.andThen (\() -> randomToTask resultGenerator)
        |> Task.perform identity


type alias UserID =
    String


type alias User =
    { id : UserID
    , name : String
    , headerImage : String
    , description : String
    , relationshipStatus : RelationshipStatus
    , sections : List UserSection
    }


type RelationshipStatus
    = Single
    | Married
    | InRelationship


type UserSection
    = Generic { header : String, content : String }
    | Images { images : List String, description : String }
    | QuestionAndAnswer { question : String, answer : String }


getUsers : List UserID -> Cmd (List User)
getUsers idsToRequest =
    let
        forID : UserID -> Generator User
        forID id =
            map (User id) Faker.name
                |> andMap Faker.imgUrl
                |> andMap Faker.paragraph
                |> andMap (uniform Single [ Married, InRelationship ])
                |> andMap (Faker.listMinMax 3 15 randomSection)

        randomSection : Generator UserSection
        randomSection =
            Random.choices randomSectionGeneric [ randomSectionImage, randomSectionQuestionAndAnswer ]

        randomSectionGeneric : Generator UserSection
        randomSectionGeneric =
            map2 (\h c -> Generic { header = h, content = c }) Faker.sentence Faker.sentence

        randomSectionImage : Generator UserSection
        randomSectionImage =
            map2 (\images descr -> Images { images = images, description = descr }) (Faker.listMinMax 1 10 Faker.imgUrl) Faker.sentence

        randomSectionQuestionAndAnswer : Generator UserSection
        randomSectionQuestionAndAnswer =
            map2 (\q a -> QuestionAndAnswer { question = q, answer = a }) Faker.question Faker.paragraph
    in
    -- IRL we'd do a backend call here (we should support batch requesting users), but for now just generate random users
    List.map forID idsToRequest
        |> List.foldr (Random.map2 (::)) (Random.constant [])
        |> fakeHttpRequest
