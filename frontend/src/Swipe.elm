port module Swipe exposing
    ( Event(..)
    , InternalMsg
    , InternalState
    , Position
    , init
    , internalUpdate
    , onSwipe
    )

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)


type InternalState
    = None
      -- We only save the start position, the current position will be part of each message
    | Starting { id : Int, startPos : Position }
    | Moving { id : Int, startPos : Position }


init : InternalState
init =
    None


type InternalMsg
    = InternalStart PointerEvent
    | InternalMove PointerEvent
    | InternalEnd PointerEvent


type Event
    = Start Position
    | Move Position Position
    | End Position Position


internalUpdate : InternalMsg -> InternalState -> ( InternalState, Maybe Event, Cmd msg )
internalUpdate msg model =
    let
        ignore =
            ( model, Nothing, Cmd.none )
    in
    case ( msg, model ) of
        ( InternalStart evnt, None ) ->
            ( Starting { id = evnt.id, startPos = evnt.position }
            , Just <| Start evnt.position
            , setPointerCapture evnt.raw
            )

        ( InternalStart _, Starting _ ) ->
            -- We've already started and starting again
            -- Likely a different point, so just ignore
            ignore

        ( InternalStart _, Moving _ ) ->
            -- We're already moving but getting another start?
            -- Likely a different point, so just ignore
            ignore

        ( InternalMove _, None ) ->
            -- We're getting a move event but haven't started the swipe
            -- Probably just the mouse moving over this element without having clicked
            -- So ignore
            ignore

        ( InternalMove evnt, Starting state ) ->
            ( Moving state
            , Just <| Move state.startPos evnt.position
            , Cmd.none
            )

        ( InternalMove evnt, Moving state ) ->
            ( Moving state
            , Just <| Move state.startPos evnt.position
            , Cmd.none
            )

        ( InternalEnd _, None ) ->
            -- Already ending but we haven't even started
            -- Just ignore
            ignore

        ( InternalEnd evnt, Starting state ) ->
            -- Our swipe has come to an end, to release capture and reset state
            ( None
            , Just <| End state.startPos evnt.position
            , releasePointerCapture evnt.raw
            )

        ( InternalEnd evnt, Moving state ) ->
            -- Our swipe has come to an end, to release capture and reset state
            ( None
            , Just <| End state.startPos evnt.position
            , releasePointerCapture evnt.raw
            )


onSwipe :
    InternalState
    -> (InternalMsg -> msg)
    -> List (Attribute msg)
onSwipe state swipeMsg =
    let
        onMapped eventName internalTagger =
            on eventName <| Decode.map (swipeMsg << internalTagger) decodePointerEvent

        startAttrs =
            [ onMapped "pointerdown" InternalStart ]

        moveOrEndAttrs =
            [ onMapped "pointermove" InternalMove
            , onMapped "pointerup" InternalEnd
            ]
    in
    case state of
        None ->
            startAttrs

        Starting _ ->
            moveOrEndAttrs

        Moving _ ->
            moveOrEndAttrs



-- PointerEvent


type alias PointerEvent =
    { raw : Decode.Value
    , id : Int
    , position : Position
    }


decodePointerEvent : Decoder PointerEvent
decodePointerEvent =
    Decode.value
        |> Decode.andThen
            (\value ->
                Decode.map3
                    (\id x y ->
                        { raw = value
                        , id = id
                        , position = { x = x, y = y }
                        }
                    )
                    (Decode.field "pointerId" Decode.int)
                    (Decode.field "clientX" Decode.float)
                    (Decode.field "clientY" Decode.float)
            )



-- Position


type alias Position =
    { x : Float, y : Float }



-- Ports


port setPointerCapture : Decode.Value -> Cmd msg


port releasePointerCapture : Decode.Value -> Cmd msg
