module BookFilter exposing (..)

import Set exposing (Set)


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


type alias State =
    -- when you add more filters, check also `isFiltered`
    { queryFilterStr : String
    , queryFilterOn : Bool
    , queryFilterMode : QueryMode
    , targetFilter : Set String
    , collFilter : CollFilter
    , kyCaseFilterOn : Bool
    }


default : State
default =
    { queryFilterStr = ""
    , queryFilterOn = False
    , queryFilterMode = And
    , targetFilter = Set.empty
    , collFilter = Both
    , kyCaseFilterOn = False
    }


isFiltered : State -> Bool
isFiltered filters =
    List.any (\b -> b)
        [ filters.queryFilterOn
        , filters.collFilter /= Both
        , filters.kyCaseFilterOn
        , not (Set.isEmpty filters.targetFilter)
        ]


type QueryMode
    = And
    | Or


qmToString : QueryMode -> String
qmToString queryMode =
    case queryMode of
        And ->
            "AND"

        Or ->
            "OR"


listAllQM : List QueryMode
listAllQM =
    [ And, Or ]


type CollFilter
    = Both
    | Own
    | ILL


cfToString : CollFilter -> String
cfToString collFilter =
    case collFilter of
        Both ->
            "全て"

        Own ->
            "自館"

        ILL ->
            "他館"


listAllCF : List CollFilter
listAllCF =
    [ Own, ILL, Both ]


type Msg
    = SetQueryFilterMode QueryMode
    | EditQueryFilter String
    | ToggleQueryFilter Bool
    | SetBookTargetFilter String
    | SetBookCollFilter CollFilter
    | ToggleKyCaseFilter Bool


update : Msg -> State -> ( State, String )
update msg state =
    case msg of
        SetQueryFilterMode mode ->
            ( { state | queryFilterMode = mode }
            , "SetQueryFilterMode " ++ qmToString mode
            )

        EditQueryFilter query ->
            ( { state | queryFilterStr = query }
            , ""
            )

        ToggleQueryFilter flag ->
            ( { state | queryFilterOn = flag }
            , "ToggleQueryFilter " ++ boolToString flag
            )

        SetBookTargetFilter bookTarget ->
            let
                targetsToString =
                    String.join ";" <| Set.toList state.targetFilter
            in
            ( { state
                | targetFilter = toggleSetMember bookTarget state.targetFilter
              }
            , "SetBookTargetFilter " ++ targetsToString
            )

        SetBookCollFilter collFilter ->
            ( { state | collFilter = collFilter }
            , "SetBookCollFilter " ++ cfToString collFilter
            )

        ToggleKyCaseFilter flag ->
            ( { state | kyCaseFilterOn = flag }
            , "ToggleKyCaseFilter " ++ boolToString flag
            )
