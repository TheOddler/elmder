module Swipe exposing
    ( Gesture
    , Position
    , attributes
    )

import Html
import Html.Attributes as Attributes
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as Json exposing (Decoder)


type alias Position =
    { x : Float, y : Float }


type RawEvent
    = TouchStart Position
    | TouchMove Position
    | TouchEnd Position


attributes : Gesture -> (Gesture -> msg) -> List (Html.Attribute msg)
attributes gestureState gestureHandler =
    List.map (Attributes.map (gestureHandler << update gestureState))
        [ on "touchstart" <| Json.map TouchStart (decodeTouchPosition "touches")
        , on "touchmove" <| Json.map TouchMove (decodeTouchPosition "changedTouches")
        , preventDefaultOn "touchend" <| Json.map (\pos -> ( TouchEnd pos, True )) (decodeTouchPosition "changedTouches")
        ]


type Gesture
    = None
    | Starting Position
    | Moving Position Position
    | Swipe Position Position


update : Gesture -> RawEvent -> Gesture
update gesture event =
    case ( gesture, event ) of
        ( None, TouchStart startPos ) ->
            Starting startPos

        ( None, TouchMove _ ) ->
            None

        ( None, TouchEnd _ ) ->
            None

        ( Starting _, TouchStart startPos ) ->
            Starting startPos

        ( Starting startPos, TouchMove curPos ) ->
            Moving startPos curPos

        ( Starting startPos, TouchEnd endPos ) ->
            Swipe startPos endPos

        ( Moving startPos curPos, TouchStart _ ) ->
            -- This is a weird event, we're getting a start event when we're already moving
            -- So we just ignore that event
            Moving startPos curPos

        ( Moving startPos _, TouchMove curPos ) ->
            Moving startPos curPos

        ( Moving startPos _, TouchEnd endPos ) ->
            Swipe startPos endPos

        ( Swipe _ _, TouchStart startPos ) ->
            -- Swipe has already finished, so we start a new one
            Starting startPos

        ( Swipe _ _, TouchMove _ ) ->
            -- We've finished but we didn't get a new start and are moving again
            -- So the swipe probably started outside this element, so we ignore this even
            None

        ( Swipe _ _, TouchEnd _ ) ->
            -- We've finished and we're finishing again with having even started a new swipe
            -- So the swipe probably started outside this element, so we ignore this even
            None


decodePosition : Decoder Position
decodePosition =
    Json.map2 Position
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


decodeTouchPosition : String -> Decoder Position
decodeTouchPosition fieldName =
    Json.at [ fieldName, "0" ] decodePosition
