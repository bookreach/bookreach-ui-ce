module Utils exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on, stopPropagationOn, targetValue)
import Json.Decode
import Set exposing (Set)



-- Custom Types and Helpers


type LocalStore a
    = NotLoaded
    | Loading
    | LoadErr
    | Loaded a


extractLS : LocalStore a -> Maybe a
extractLS localStore =
    case localStore of
        Loaded value ->
            Just value

        _ ->
            Nothing


toggleId : { a | id : comparable } -> Set comparable -> Set comparable
toggleId selected registeredIds =
    toggleSetMember selected.id registeredIds



-- Events


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (targetValue |> Json.Decode.map tagger)


onClickSP : msg -> Attribute msg
onClickSP msg =
    stopPropagationOn "click" (Json.Decode.succeed ( msg, True ))



-- Basics


toggleSetMember : comparable -> Set comparable -> Set comparable
toggleSetMember comp aSet =
    if Set.member comp aSet then
        Set.remove comp aSet

    else
        Set.insert comp aSet


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"
