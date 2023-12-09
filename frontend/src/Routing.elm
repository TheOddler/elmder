module Routing exposing (..)

import Enums exposing (allImpressions, reverseEnumToString)
import Generated.Backend exposing (Impression(..), UserID)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, custom, int, map, oneOf, parse, s, top)


type Route
    = RouteSearch
    | RouteImpression Impression
    | RouteMyProfile
    | RouteOtherUser UserID


routeUrlParser : Parser (Route -> a) a
routeUrlParser =
    oneOf
        [ map RouteSearch top
        , map RouteImpression <|
            custom "IMPRESSION" <|
                reverseEnumToString allImpressions impressionToUrlSegement
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

        RouteImpression impression ->
            absolute [ impressionToUrlSegement impression ] []

        RouteMyProfile ->
            absolute [ "settings" ] []

        RouteOtherUser userID ->
            absolute [ "user", String.fromInt userID ] []


impressionToUrlSegement : Impression -> String
impressionToUrlSegement impression =
    case impression of
        ImpressionLike ->
            "likes"

        ImpressionDislike ->
            "dislikes"

        ImpressionDecideLater ->
            "decide-later"

        ImpressionSuperLike ->
            "super-likes"
