module Swipe exposing
    (  Gesture(..)
       -- , Position

    , attributes
    , init
    )

import Html
import Html.Attributes as Attributes
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)


type alias Position =
    { x : Float, y : Float }


type RawEvent
    = PointerDown Position
    | PointerMove Position
    | PointerUp Position


attributes : Gesture -> (Gesture -> msg) -> List (Html.Attribute msg)
attributes gestureState gestureHandler =
    List.map (Attributes.map (gestureHandler << update gestureState))
        [ on "pointerdown" <| Decode.map PointerDown decodePosition
        , on "pointermove" <| Decode.map PointerMove decodePosition
        , on "pointerup" <| Decode.map PointerUp decodePosition
        ]


type Gesture
    = None
    | Start Position
    | Moving Position Position
    | Ended Position Position


init : Gesture
init =
    None


update : Gesture -> RawEvent -> Gesture
update gesture event =
    case ( gesture, event ) of
        ( None, PointerDown startPos ) ->
            Start startPos

        ( None, PointerMove _ ) ->
            None

        ( None, PointerUp _ ) ->
            None

        ( Start _, PointerDown startPos ) ->
            Start startPos

        ( Start startPos, PointerMove curPos ) ->
            Moving startPos curPos

        ( Start startPos, PointerUp endPos ) ->
            Ended startPos endPos

        ( Moving startPos curPos, PointerDown _ ) ->
            -- This is a weird event, we're getting a start event when we're already moving
            -- So we just ignore that event
            Moving startPos curPos

        ( Moving startPos _, PointerMove curPos ) ->
            Moving startPos curPos

        ( Moving startPos _, PointerUp endPos ) ->
            Ended startPos endPos

        ( Ended _ _, PointerDown startPos ) ->
            -- Swipe has already finished, so we start a new one
            Start startPos

        ( Ended _ _, PointerMove _ ) ->
            -- We've finished but we didn't get a new start and are moving again
            -- So the swipe probably started outside this element, so we ignore this even
            None

        ( Ended _ _, PointerUp _ ) ->
            -- We've finished and we're finishing again with having even started a new swipe
            -- So the swipe probably started outside this element, so we ignore this even
            None


decodePosition : Decoder Position
decodePosition =
    Decode.map2 Position
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
