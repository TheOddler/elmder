module Routing exposing (..)

import Generated.Backend exposing (Impression(..), UserID)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, top)


type Route
    = RouteSearch
    | RouteLikesAndSuperLikes
    | RouteDislikes
    | RouteDecideLater
    | RouteMatches
    | RouteAdmirers
    | RouteMyProfile
    | RouteOtherUser UserID


routeUrlParser : Parser (Route -> a) a
routeUrlParser =
    oneOf
        [ map RouteSearch top
        , map RouteLikesAndSuperLikes (s "likes")
        , map RouteDislikes (s "dislikes")
        , map RouteDecideLater (s "decide-later")
        , map RouteMatches (s "matches")
        , map RouteAdmirers (s "admirers")
        , map RouteMyProfile (s "settings")
        , map RouteOtherUser (s "user" </> int)
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    parse routeUrlParser url


routeToUrl : Route -> String
routeToUrl route =
    case route of
        RouteSearch ->
            absolute [] []

        RouteLikesAndSuperLikes ->
            absolute [ "likes" ] []

        RouteDislikes ->
            absolute [ "dislikes" ] []

        RouteDecideLater ->
            absolute [ "decide-later" ] []

        RouteMatches ->
            absolute [ "matches" ] []

        RouteAdmirers ->
            absolute [ "admirers" ] []

        RouteMyProfile ->
            absolute [ "settings" ] []

        RouteOtherUser userID ->
            absolute [ "user", String.fromInt userID ] []
