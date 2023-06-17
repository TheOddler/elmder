module User.Random exposing (..)

import Faker exposing (..)
import Random exposing (..)
import Random.Char exposing (..)
import Random.Extra exposing (..)
import Random.String exposing (..)
import User exposing (..)


random : Generator User
random =
    let
        id =
            string 10 lowerCaseLatin

        listMinMax : Int -> Int -> Generator a -> Generator (List a)
        listMinMax min max gen =
            int min max
                |> andThen (\len -> list len gen)
    in
    map User id
        |> andMap name
        |> andMap (listMinMax 1 10 imgUrl)
        |> andMap paragraph
        |> andMap (uniform Single [ Married, InRelationship ])
        |> andMap (listMinMax 3 15 randomSection)


randomSection : Generator UserSection
randomSection =
    choices randomSectionGeneric [ randomSectionImage, randomSectionQuestionAndAnswer ]


randomSectionGeneric : Generator UserSection
randomSectionGeneric =
    map2 (\h c -> Generic { header = h, content = c }) sentence sentence


randomSectionImage : Generator UserSection
randomSectionImage =
    map2 (\u d -> Image { url = u, description = d }) imgUrl sentence


randomSectionQuestionAndAnswer : Generator UserSection
randomSectionQuestionAndAnswer =
    map2 (\q a -> QuestionAndAnswer { question = q, answer = a }) question paragraph
