module BookFilter exposing (Msg, State, apply, default, isFiltered, update, view)

import Api exposing (Book, Mapping, mappingToLibraries)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)
import Utils exposing (toggleSetMember)



-- Model


type alias State =
    { query : String
    , queryOn : Bool
    , queryOperator : Operator
    , libraries : Set Int
    }


type Operator
    = And
    | Or


default : State
default =
    { query = ""
    , queryOn = False
    , queryOperator = And
    , libraries = Set.empty
    }



-- Helper


isFiltered : State -> Bool
isFiltered filters =
    List.any (\b -> b)
        [ filters.queryOn
        , not (Set.isEmpty filters.libraries)
        ]


apply : State -> List Book -> List Book
apply state bookList =
    bookList
        |> filterBooksByQuery state.queryOn state.queryOperator state.query
        |> filterBooksByLibrary state.libraries


qmToString : Operator -> String
qmToString queryMode =
    case queryMode of
        And ->
            "AND"

        Or ->
            "OR"


listAllQM : List Operator
listAllQM =
    [ And, Or ]



-- Update


type Msg
    = SetQueryOperator Operator
    | EditQuery String
    | ToggleQueryFilter Bool
    | SetLibraryFilter Int


update : Msg -> State -> State
update msg state =
    case msg of
        SetQueryOperator mode ->
            { state | queryOperator = mode }

        EditQuery query ->
            { state | query = query }

        ToggleQueryFilter flag ->
            { state | queryOn = flag }

        SetLibraryFilter libId ->
            { state | libraries = toggleSetMember libId state.libraries }



-- Filtering logic


filterBooksByQuery : Bool -> Operator -> String -> List Book -> List Book
filterBooksByQuery flag mode queryInput books =
    let
        queries =
            String.split " " <| String.replace "\u{3000}" " " queryInput

        checkEachQueryOR book =
            queries
                |> List.any (\query -> String.contains query (bookContentForSearch book))

        checkEachQueryAND book =
            queries
                |> List.all (\query -> String.contains query (bookContentForSearch book))
    in
    case ( flag, mode ) of
        ( True, And ) ->
            List.filter checkEachQueryAND books

        ( True, Or ) ->
            List.filter checkEachQueryOR books

        ( False, _ ) ->
            books


bookContentForSearch : Book -> String
bookContentForSearch book =
    String.join " "
        [ book.title
        , book.author
        , book.publisher
        , book.pubdate
        , book.volume
        ]


filterBooksByLibrary : Set Int -> List Book -> List Book
filterBooksByLibrary collFilter books =
    let
        matchLibraryFilter : Book -> Bool
        matchLibraryFilter book =
            not (Set.isEmpty <| Set.intersect (Set.fromList book.holdings) collFilter)
    in
    if Set.isEmpty collFilter then
        books

    else
        List.filter matchLibraryFilter books



-- View


view : State -> Mapping -> Html Msg
view filters mapping =
    nav [ class "level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ] [ viewBookLibraryFilter filters mapping ]
            , div [ class "level-item" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-sort-amount-down" ] [] ]
                    , span [] [ text "出版年が新しい順" ]
                    ]
                ]
            ]
        , div [ class "level-right" ]
            [ div [ class "level-item" ]
                [ div [ class "field has-addons" ] <|
                    viewFilterQueryModeButtons filters.queryOperator
                        ++ [ p [ class "control" ]
                                [ p [ class "control" ]
                                    [ input
                                        [ class "input is-small"
                                        , type_ "text"
                                        , placeholder "書誌情報に …… を含む"
                                        , value filters.query
                                        , onInput EditQuery
                                        ]
                                        []
                                    ]
                                ]
                           , p [ class "control" ]
                                [ button
                                    [ classList
                                        [ ( "button is-small", True )
                                        , ( "is-danger is-outlined", filters.queryOn )
                                        ]
                                    , onClick <| ToggleQueryFilter (not filters.queryOn)
                                    ]
                                    [ text <|
                                        if filters.queryOn then
                                            "絞込解除"

                                        else
                                            "絞り込む"
                                    ]
                                ]
                           ]
                ]
            ]
        ]


viewBookLibraryFilter : State -> Mapping -> Html Msg
viewBookLibraryFilter state mapping =
    let
        collFilter =
            state.libraries

        libraries =
            mappingToLibraries mapping
    in
    div [ class "level-item" ]
        [ div [ class "dropdown is-hoverable" ]
            [ div [ class "dropdown-trigger" ]
                [ button
                    [ class "button"
                    , classList [ ( "is-info", not (Set.isEmpty collFilter) ) ]
                    , attribute "aria-haspopup" "true"
                    , attribute "aria-controls" "collfilter-menu"
                    ]
                    [ span [ class "icon is-small" ] [ i [ class "fas fa-university" ] [] ]
                    , span [ class "is-small" ] [ text "所蔵館" ]
                    , span [ class "icon is-small" ] [ i [ class "fas fa-angle-down", attribute "aria-hidden" "true" ] [] ]
                    ]
                ]
            , div
                [ class "dropdown-menu"
                , id "collfilter-menu"
                , attribute "role" "menu"
                ]
                [ div
                    [ class "dropdown-content"
                    , style "max-height" "14em"
                    , style "overflow" "auto"
                    ]
                    (List.map (viewBookLibraryFilterItem collFilter) libraries)
                ]
            ]
        ]


viewBookLibraryFilterItem : Set Int -> ( Int, String ) -> Html Msg
viewBookLibraryFilterItem currentLibraryFilter ( libId, libName ) =
    div [ class "dropdown-item" ]
        [ label [ class "checkbox" ]
            [ input
                [ type_ "checkbox"
                , onClick (SetLibraryFilter libId)
                , checked (Set.member libId currentLibraryFilter)
                ]
                []
            , span [ class "ml-1" ] [ text libName ]
            ]
        ]


viewFilterQueryModeButtons : Operator -> List (Html Msg)
viewFilterQueryModeButtons queryMode =
    let
        viewFQMButton mode =
            div [ class "control" ]
                [ button
                    [ classList
                        [ ( "button is-small", True )
                        , ( "is-active", queryMode == mode )
                        ]
                    , onClick (SetQueryOperator mode)
                    ]
                    [ text <| qmToString mode ]
                ]
    in
    List.map viewFQMButton listAllQM
