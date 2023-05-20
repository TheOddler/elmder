module Faker exposing (..)

import Random exposing (..)
import Random.Char exposing (..)
import Random.Extra exposing (..)
import Random.String exposing (..)
import String.Extra as String


imgUrl : Generator String
imgUrl =
    let
        mkImgUrl width height rnd =
            "http://placekitten.com/" ++ String.fromInt width ++ "/" ++ String.fromInt height ++ "?" ++ String.fromInt rnd
    in
    map3 mkImgUrl (int 200 600) (int 200 600) (int minInt maxInt)


name : Generator String
name =
    let
        mkName prefix givenName =
            prefix ++ " " ++ givenName
    in
    map2
        mkName
        (uniform "Ms." [ "Mr.", "Miss.", "Dr.", "" ])
        (uniform "Liam"
            [ "Olivia"
            , "Noah"
            , "Emma"
            , "Oliver"
            , "Charlotte"
            , "James"
            , "Amelia"
            , "Elijah"
            , "Sophia"
            , "William"
            , "Isabella"
            , "Henry"
            , "Ava"
            , "Lucas"
            , "Mia"
            , "Benjamin"
            , "Evelyn"
            , "Theodore"
            , "Luna"
            ]
        )


paragraph : Generator String
paragraph =
    int 2 8
        |> andThen (\length -> list length sentence)
        |> andThen (\sentences -> constant <| String.join " " sentences)


sentence : Generator String
sentence =
    int 3 10
        |> andThen (\length -> list length word)
        |> map (\words -> String.toSentenceCase <| String.join " " words ++ ".")


word : Generator String
word =
    uniform "time"
        [ "be"
        , "good"
        , "to"
        , "the"
        , "person"
        , "have"
        , "new"
        , "of"
        , "and"
        , "year"
        , "do"
        , "first"
        , "in"
        , "a"
        , "way"
        , "say"
        , "last"
        , "for"
        , "that"
        , "day"
        , "get"
        , "long"
        , "on"
        , "I"
        , "thing"
        , "make"
        , "great"
        , "with"
        , "it"
        , "man"
        , "go"
        , "little"
        , "at"
        , "not"
        , "world"
        , "know"
        , "own"
        , "by"
        , "he"
        , "life"
        , "take"
        , "other"
        , "from"
        , "as"
        , "hand"
        , "see"
        , "old"
        , "up"
        , "you"
        , "part"
        , "come"
        , "right"
        , "about"
        , "this"
        , "child"
        , "think"
        , "big"
        , "into"
        , "but"
        , "eye"
        , "look"
        , "high"
        , "over"
        , "his"
        , "woman"
        , "want"
        , "different"
        , "after"
        , "they"
        , "place"
        , "give"
        , "small"
        , "her"
        , "work"
        , "use"
        , "large"
        , "she"
        , "week"
        , "find"
        , "next"
        , "or"
        , "case"
        , "tell"
        , "early"
        , "an"
        , "point"
        , "ask"
        , "young"
        , "will"
        , "government"
        , "work"
        , "important"
        , "my"
        , "company"
        , "seem"
        , "few"
        , "one"
        , "number"
        , "feel"
        , "public"
        , "all"
        , "group"
        , "try"
        , "bad"
        , "would"
        , "problem"
        , "leave"
        , "same"
        , "there"
        , "fact"
        , "call"
        , "able"
        , "their"
        ]
