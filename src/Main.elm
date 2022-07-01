port module Main exposing (..)

import BookDB exposing (..)
import BookFilter exposing (CollFilter(..), Msg(..), QueryMode(..), cfToString, isFiltered, listAllCF, listAllQM, qmToString, toggleSetMember)
import Browser
import Csv.Encode as CsvEn
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html, a, button, div, figure, footer, h1, h2, header, i, img, input, label, li, nav, option, p, section, select, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, colspan, disabled, href, id, placeholder, rowspan, selected, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Http
import List.Extra as ListEx
import School exposing (School(..))
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port requestPrint : Bool -> Cmd msg


{-| Tell the modal's state (open or close) to switch 'is-clipped' attribute of <html> so that the background stops scrolling while a modal is opened.
-}
port modalState : Bool -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL HELPERS


type BookModal
    = Closed
    | Opened Book


type FetchStatus
    = NotCached
    | Fetching
    | FetchErr
    | Fetched


type LocalStore carrier
    = NotLoaded
    | Loading
    | LoadErr
    | Loaded carrier


type alias TextBookStore =
    LocalStore (List TextBook)


type alias TangenStore =
    LocalStore (List Tangen)


type alias ChapterStore =
    LocalStore (List Chapter)


type alias NdcLabelStore =
    -- NDC (String) to Label/Text (String)
    LocalStore (Dict String String)


type alias BookStore =
    -- NDC (String) to Books with the NDC
    -- BookStore FetchStatus (Dict String (List Book))
    LocalStore (Dict String (List Book))



-- MODEL


type alias Model =
    { selectedSchool : Maybe School
    , selectedTextBook : Maybe TextBook
    , selectedChapterIds : Set Int
    , selectedNdcs : Set String

    -- book browser state
    , selectedBookIds : Set Int
    , bookModal : BookModal
    , selectedNdcTab : String
    , selectedPagination : Int

    -- advSearch state
    , advSearchModalOpened : Bool
    , advSearchQuery : String
    , advSearchNdc : String
    , advSearchStatus : FetchStatus

    -- filtering state
    , filters : BookFilter.State

    -- database stores
    , textBookStore : TextBookStore
    , tangenStore : TangenStore
    , chapterStore : ChapterStore
    , ndcLabelStore : NdcLabelStore
    , bookStore : BookStore
    , kyCases : List KyCase
    }


initModel : Model
initModel =
    { selectedSchool = Nothing
    , selectedTextBook = Nothing
    , selectedChapterIds = Set.empty
    , selectedNdcs = Set.empty
    , selectedBookIds = Set.empty
    , bookModal = Closed
    , selectedNdcTab = ""
    , selectedPagination = 1
    , advSearchModalOpened = False
    , advSearchQuery = ""
    , advSearchNdc = ""
    , advSearchStatus = NotCached
    , filters = BookFilter.default
    , textBookStore = NotLoaded
    , tangenStore = NotLoaded
    , chapterStore = NotLoaded
    , ndcLabelStore = NotLoaded
    , bookStore = NotLoaded
    , kyCases = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type
    Msg
    -- Initial setup
    = SelectSchool School
    | GotTextBooks (Result Http.Error (List TextBook))
    | GotTextBook (Result Http.Error TextBook)
    | SelectTextBook TextBook
    | GotTangens (Result Http.Error (List Tangen))
    | SelectTangen Tangen
    | SelectChapter Chapter
    | FetchBooks
    | GotNdcLabels (Result Http.Error (List NdcLabel))
    | GotBooks (Result Http.Error (List Book))
      -- Book Browser UI
    | SelectNdcTab String
    | SelectedPagination Int
    | ToggleBookModal BookModal
    | GotKyCases (Result Http.Error (List KyCase))
    | SelectBook Book
      -- AdvSearch
    | ToggleAdvSearchModal
    | EditAdvSearchQuery String
    | SelectAdvSearchNdc String
    | RequestAdvSearch
    | GotAdvSearchResult (Result Http.Error (List Book))
    | GotAdvSearchResultInit (Maybe String) (Maybe String) (Result Http.Error (List Book))
      -- Filter UI
    | BookFilterMsg BookFilter.Msg
      -- Export data
    | RequestTsv
    | RequestCsv
    | RequestPrint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Initial setup
        SelectSchool school ->
            ( { initModel | selectedSchool = Just school }
            , getTextBooks GotTextBooks school
            )

        GotTextBooks result ->
            case result of
                Ok textBooks ->
                    ( { model | textBookStore = Loaded textBooks }, Cmd.none )

                Err _ ->
                    ( { model | textBookStore = LoadErr }, Cmd.none )

        GotTextBook result ->
            case result of
                Ok textBook ->
                    ( { model | selectedTextBook = Just textBook }, Cmd.none )

                Err _ ->
                    ( { model | selectedTextBook = Nothing }, Cmd.none )

        SelectTextBook textBook ->
            ( { initModel
                | selectedSchool = model.selectedSchool
                , selectedTextBook = Just textBook
                , textBookStore = model.textBookStore
              }
            , getTangens GotTangens textBook.id
            )

        GotTangens result ->
            case result of
                Ok tangens ->
                    let
                        newModel =
                            { model
                                | tangenStore = Loaded tangens
                                , chapterStore = Loaded <| List.concat (List.map .chapters tangens)
                            }
                    in
                    ( newModel, Cmd.none )

                Err _ ->
                    ( { model | tangenStore = LoadErr }, Cmd.none )

        SelectTangen tangen ->
            let
                tangenChapterIds =
                    Set.fromList <| List.map .id tangen.chapters

                newSelectedChapterIds =
                    if isAllChapterSelected model.selectedChapterIds tangen.chapters then
                        Set.diff model.selectedChapterIds tangenChapterIds

                    else
                        Set.union model.selectedChapterIds tangenChapterIds
            in
            ( { model | selectedChapterIds = newSelectedChapterIds }
            , Cmd.none
            )

        SelectChapter chapter ->
            ( { model | selectedChapterIds = toggleId chapter.id model.selectedChapterIds }
            , Cmd.none
            )

        FetchBooks ->
            let
                selectedNdcs =
                    mapSelectedChaptersToNdcs model.chapterStore model.selectedChapterIds
            in
            ( { model | selectedNdcs = selectedNdcs, bookStore = Loading }
            , Cmd.batch
                [ getBooks GotBooks selectedNdcs
                , getAllNdcLabels GotNdcLabels
                ]
            )

        GotNdcLabels result ->
            case result of
                Ok ndcLabels ->
                    let
                        ndcLabelDict =
                            ndcLabels
                                |> List.map (\nl -> ( nl.ndc, nl.label ))
                                |> Dict.fromList
                    in
                    ( { model | ndcLabelStore = Loaded ndcLabelDict }, Cmd.none )

                Err _ ->
                    ( { model | ndcLabelStore = LoadErr }, Cmd.none )

        GotBooks result ->
            case result of
                Ok books ->
                    let
                        ndcBookDict =
                            organiseBooksByNdc model.selectedNdcs books

                        defaultSelectedTab =
                            sortedNdcCountList ndcBookDict
                                |> List.head
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault ""
                    in
                    ( { model
                        | bookStore = Loaded ndcBookDict
                        , selectedNdcTab = defaultSelectedTab
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | bookStore = LoadErr }, Cmd.none )

        -- Book Browser UI
        SelectNdcTab ndc ->
            ( { model | selectedNdcTab = ndc, selectedPagination = 1 }, Cmd.none )

        SelectedPagination numPage ->
            ( { model | selectedPagination = numPage }, Cmd.none )

        ToggleBookModal bookModal ->
            ( { model | bookModal = bookModal }
            , case bookModal of
                Opened book ->
                    if List.isEmpty book.caseIds then
                        modalState True

                    else
                        Cmd.batch
                            [ getKyCase GotKyCases book.caseIds
                            , modalState True
                            ]

                Closed ->
                    modalState False
            )

        GotKyCases result ->
            case result of
                Ok kyCases ->
                    ( { model | kyCases = kyCases }, Cmd.none )

                Err _ ->
                    ( { model | kyCases = [] }, Cmd.none )

        SelectBook book ->
            case model.bookModal of
                Opened _ ->
                    ( { model
                        | selectedBookIds = toggleId book.id model.selectedBookIds
                        , bookModal = Closed
                      }
                    , modalState False
                    )

                Closed ->
                    ( { model | selectedBookIds = toggleId book.id model.selectedBookIds }
                    , Cmd.none
                    )

        ToggleAdvSearchModal ->
            ( { model | advSearchModalOpened = not model.advSearchModalOpened }
            , modalState (not model.advSearchModalOpened)
            )

        EditAdvSearchQuery query ->
            ( { model | advSearchQuery = query }, Cmd.none )

        SelectAdvSearchNdc ndc ->
            ( { model | advSearchNdc = ndc }, Cmd.none )

        RequestAdvSearch ->
            ( { model | advSearchStatus = Fetching }
            , case ( model.advSearchQuery, model.advSearchNdc ) of
                ( _, "" ) ->
                    getBooksByQuery GotAdvSearchResult model.advSearchQuery

                ( "", _ ) ->
                    getBooks GotAdvSearchResult (Set.fromList [ model.advSearchNdc ])

                ( _, _ ) ->
                    getBooksWithQuery GotAdvSearchResult model.advSearchNdc model.advSearchQuery
            )

        GotAdvSearchResult result ->
            case result of
                Err _ ->
                    ( { model | advSearchStatus = FetchErr }, Cmd.none )

                Ok books ->
                    case books of
                        [] ->
                            ( { model | advSearchStatus = FetchErr }, Cmd.none )

                        _ ->
                            let
                                resultKey =
                                    textAdvSearch model.ndcLabelStore model.advSearchQuery model.advSearchNdc

                                updatedBookStore =
                                    case model.bookStore of
                                        Loaded bookDict ->
                                            Loaded (Dict.insert resultKey books bookDict)

                                        _ ->
                                            Loaded (Dict.fromList [ ( resultKey, books ) ])
                            in
                            ( { model
                                | advSearchNdc = ""
                                , advSearchQuery = ""
                                , advSearchStatus = Fetched
                                , advSearchModalOpened = False
                                , bookStore = updatedBookStore
                                , selectedNdcTab = resultKey
                                , selectedPagination = 1
                              }
                            , modalState False
                            )

        GotAdvSearchResultInit maybeQuery maybeNdc result ->
            case result of
                Err _ ->
                    ( { model | advSearchStatus = FetchErr }, Cmd.none )

                Ok books ->
                    case books of
                        [] ->
                            ( { model | advSearchStatus = FetchErr }, Cmd.none )

                        _ ->
                            let
                                resultKey =
                                    textAdvSearch
                                        model.ndcLabelStore
                                        (Maybe.withDefault "" maybeQuery)
                                        (Maybe.withDefault "" maybeNdc)

                                updatedBookStore =
                                    case model.bookStore of
                                        Loaded bookDict ->
                                            Loaded (Dict.insert resultKey books bookDict)

                                        _ ->
                                            Loaded (Dict.fromList [ ( resultKey, books ) ])
                            in
                            ( { model
                                | bookStore = updatedBookStore
                                , selectedNdcTab = resultKey
                                , selectedPagination = 1
                              }
                            , Cmd.none
                            )

        -- Filter UI
        BookFilterMsg bookFilterMsg ->
            let
                ( newFilter, bookFilterLog ) =
                    BookFilter.update bookFilterMsg model.filters
            in
            if bookFilterLog == "" then
                ( { model | filters = newFilter }, Cmd.none )

            else
                ( { model | filters = newFilter, selectedPagination = 1 }, Cmd.none )

        -- Export data
        RequestTsv ->
            ( model, Cmd.batch [ exportTsv model, Cmd.none ] )

        RequestCsv ->
            ( model, Cmd.batch [ exportCsv model, Cmd.none ] )

        RequestPrint ->
            ( model, Cmd.batch [ requestPrint True, Cmd.none ] )



-- Update Helpers


toggleId : Int -> Set Int -> Set Int
toggleId selectedId registeredIds =
    toggleSetMember selectedId registeredIds


isAllChapterSelected : Set Int -> List Chapter -> Bool
isAllChapterSelected selectedChapterIds chapters =
    List.all (\chp -> Set.member chp.id selectedChapterIds) chapters


mapSelectedChaptersToNdcs : ChapterStore -> Set Int -> Set String
mapSelectedChaptersToNdcs chapterStore selectedChapterIds =
    case chapterStore of
        Loaded chapters ->
            chapters
                |> List.filter (\chp -> Set.member chp.id selectedChapterIds)
                |> List.map (\chp -> chp.ndcs)
                |> List.concat
                |> Set.fromList

        _ ->
            Set.empty


countBooksPerNdc : Dict String (List Book) -> Dict String Int
countBooksPerNdc ndcBookDict =
    Dict.map (\_ bookList -> List.length bookList) ndcBookDict


sortedNdcCountList : Dict String (List Book) -> List ( String, Int )
sortedNdcCountList ndcBookDict =
    countBooksPerNdc ndcBookDict
        |> Dict.toList
        |> List.filter ((/=) 0 << Tuple.second)
        |> List.sortBy Tuple.first


mapSortedNdcCountList : BookStore -> List ( String, Int )
mapSortedNdcCountList bookStore =
    case bookStore of
        Loaded ndcBookDict ->
            let
                locFilteredNdcBookDict =
                    Dict.map (\_ books -> filterBooksByBookColl Both books) ndcBookDict
            in
            sortedNdcCountList locFilteredNdcBookDict

        -- unreacheable
        _ ->
            []


organiseBooksByNdc : Set String -> List Book -> Dict String (List Book)
organiseBooksByNdc selectedNdcs books =
    let
        selectedNdcList =
            Set.toList selectedNdcs

        perNdcBooks =
            selectedNdcList
                |> List.map
                    (\ndc ->
                        List.filter
                            (\book -> book.ndc == ndc)
                            books
                    )
                |> List.map
                    (\bookList ->
                        List.reverse <| List.sortBy .year bookList
                    )
    in
    Dict.fromList <|
        List.map2 Tuple.pair selectedNdcList perNdcBooks


lookupNdcLabel : String -> NdcLabelStore -> String
lookupNdcLabel ndc ndcLabelStore =
    case ndcLabelStore of
        Loaded ndcLabelDict ->
            Maybe.withDefault "" (Dict.get ndc ndcLabelDict)

        _ ->
            ""


textAdvSearch : NdcLabelStore -> String -> String -> String
textAdvSearch ndcLabelStore query ndc =
    case ( query, ndc ) of
        ( _, "" ) ->
            "“" ++ query ++ "”"

        ( "", _ ) ->
            -- viewNdcTab will add NdcLabel for this case
            ndc

        ( _, _ ) ->
            String.join " "
                [ "“" ++ query ++ "”"
                , "in"
                , ndc
                , lookupNdcLabel ndc ndcLabelStore
                ]


listSelectedBooks : Set Int -> BookStore -> List Book
listSelectedBooks selectedBookIds bookStore =
    case bookStore of
        Loaded perNdcBookDict ->
            perNdcBookDict
                |> Dict.values
                |> List.concat
                |> List.map (\book -> ( book.id, book ))
                |> Dict.fromList
                |> Dict.filter (\k _ -> Set.member k selectedBookIds)
                |> Dict.values

        _ ->
            []


fileExportHeader : List String
fileExportHeader =
    [ "タイトル"
    , "副題"
    , "著者"
    , "出版年"
    , "出版社"
    , "NDC"
    , "NDC詳細"
    , "対象学年"
    , "ISBN13"
    , "ページ数"
    , "大きさ"
    ]


exportBookToStrList : Book -> List String
exportBookToStrList book =
    [ book.title
    , book.subtitle
    , String.join "・" book.authors
    , String.fromInt book.year ++ "年"
    , String.join "・" book.publishers
    , book.ndc
    , book.ndc_full
    , book.target
    , book.isbn13
    , book.pages
    , book.size
    ]


exportTsv : Model -> Cmd Msg
exportTsv model =
    let
        tsvString =
            listSelectedBooks model.selectedBookIds model.bookStore
                |> List.map bookTsvRow
                |> String.join "\n"

        bookTsvRow book =
            String.join "\t" <| exportBookToStrList book

        tsvHeader =
            String.join "\t" fileExportHeader
    in
    Download.string "bookreach-output.tsv" "text/tab-separated-values" <|
        (tsvHeader ++ "\n" ++ tsvString)


exportCsv : Model -> Cmd Msg
exportCsv model =
    let
        csvString =
            listSelectedBooks model.selectedBookIds model.bookStore
                |> List.map exportBookToStrList
                |> CsvEn.encode
                    { encoder = CsvEn.withFieldNames (ListEx.zip fileExportHeader)
                    , fieldSeparator = ','
                    }
    in
    Download.string "bookreach-output.csv" "text/csv" csvString



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHero

        -- Initialisation UI
        , section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ viewSchoolSelector model.selectedSchool
                    , viewTextBookSelector model.selectedTextBook model.textBookStore
                    ]
                , viewTangenChapterSelector model.selectedChapterIds model.tangenStore model.bookStore
                ]
            ]

        -- Modal windows
        , viewBookModal model
        , viewAdvSearchModal model

        -- Book Browser; Material list
        , section [ class "section no-print" ]
            [ div [ class "container" ] <|
                [ h2 [ class "title" ] [ text "教材候補" ]
                , viewBookFilterControl model.filters
                ]
                    -- NDC-based lists of books
                    ++ viewBookBrowser model
            ]

        -- Table output
        , viewTableOutput model.selectedBookIds model.bookStore
        ]


viewHero : Html Msg
viewHero =
    section [ class "hero is-info no-print" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-three-fifths is-offset-one-fifth" ]
                        [ h1 [ class "title has-text-centered" ]
                            [ text "単元探索" ]
                        , p [ class "subtitle has-text-centered" ]
                            [ text "単元に合わせて関連した蔵書を見つけます。" ]
                        ]
                    ]
                ]
            ]
        ]


viewSchoolSelector : Maybe School -> Html Msg
viewSchoolSelector selectedSchool =
    div [ class "column is-4" ]
        [ h2 [ class "title no-print" ] [ text "校種" ]
        , p [ class "subtitle no-print" ] [ text "校種を選択してください" ]
        , p [ class "buttons" ] <|
            List.map (viewSchoolButton selectedSchool) School.all
        ]


viewSchoolButton : Maybe School -> School -> Html Msg
viewSchoolButton maybeSchool school =
    let
        schoolButtonClasses =
            case maybeSchool of
                Just selectedSchool ->
                    [ ( "is-info", selectedSchool == school )
                    , ( "no-print", selectedSchool /= school )
                    ]

                Nothing ->
                    []
    in
    button
        [ class "button"
        , classList schoolButtonClasses
        , onClick (SelectSchool school)

        -- currently we don't support highschools
        , disabled (school == High)
        ]
        [ text (School.toString school) ]


viewTextBookSelector : Maybe TextBook -> TextBookStore -> Html Msg
viewTextBookSelector selectedTextBook textBookStore =
    div [ class "column" ]
        [ h2 [ class "title no-print" ] [ text "教科書" ]
        , p [ class "subtitle no-print" ] [ text "教科書を選択してください" ]
        , p [ class "buttons" ] <|
            case textBookStore of
                LoadErr ->
                    [ button [ class "button is-danger" ] [ text "サーバ接続エラー：ページを再読込してください" ] ]

                Loading ->
                    [ button [ class "button is-loading" ] [ text "読み込み中" ] ]

                Loaded textBooks ->
                    List.map (viewTextBookButton selectedTextBook) textBooks

                NotLoaded ->
                    []
        ]


viewTextBookButton : Maybe TextBook -> TextBook -> Html Msg
viewTextBookButton selectedTextBook textBook =
    let
        textBookClasses =
            case selectedTextBook of
                Just textBookSelected ->
                    [ ( "is-info", textBookSelected.id == textBook.id )
                    , ( "no-print", textBookSelected.id /= textBook.id )
                    ]

                Nothing ->
                    []
    in
    button
        [ class "button"
        , classList textBookClasses
        , onClick (SelectTextBook textBook)
        ]
        [ text textBook.title ]


viewTangenChapterSelector : Set Int -> TangenStore -> BookStore -> Html Msg
viewTangenChapterSelector selectedChapterIds tangenStore bookStore =
    div [ class "columns" ]
        [ div [ class "column", class "is-full" ]
            [ h2 [ class "title no-print" ] [ text "単元・章" ]
            , p [ class "subtitle no-print" ] [ text "章を選択してください" ]
            , viewTangenChapterTable selectedChapterIds tangenStore
            , button
                [ class "button is-success no-print"
                , classList [ ( "is-loading", bookStore == Loading ) ]
                , onClick FetchBooks
                , disabled (Set.isEmpty selectedChapterIds)
                ]
                [ text "取得" ]
            ]
        ]


viewTangenChapterTable : Set Int -> TangenStore -> Html Msg
viewTangenChapterTable selectedChapterIds tangenStore =
    table [ class "table is-fullwidth is-hoverable" ]
        [ thead []
            [ th [ class "has-text-centered" ] [ text "単元" ]
            , th [ class "has-text-left" ] [ text "単元名" ]
            , th [ class "has-text-left" ] [ text "章" ]
            , th [ class "has-text-centered" ] [ text "" ]
            ]
        , tbody [] <|
            case tangenStore of
                NotLoaded ->
                    []

                Loading ->
                    [ tr [] [ td [ colspan 4 ] [ text "読み込み中" ] ] ]

                LoadErr ->
                    [ tr [] [ td [ colspan 4 ] [ text "DBエラー" ] ] ]

                Loaded tangens ->
                    List.concat <| List.map (viewTangenChapterRow selectedChapterIds) tangens
        ]


viewTangenChapterRow : Set Int -> Tangen -> List (Html Msg)
viewTangenChapterRow selectedChapterIds tangen =
    viewTangenChapterRowFirst selectedChapterIds tangen
        :: viewTangenChapterRowRest selectedChapterIds tangen


viewTangenChapterRowFirst : Set Int -> Tangen -> Html Msg
viewTangenChapterRowFirst selectedChapterIds tangen =
    let
        tangenClasses =
            [ rowspan (List.length tangen.chapters)
            , classList
                [ ( "is-vcentered", True )
                , ( "is-info", isAllChapterSelected selectedChapterIds tangen.chapters )
                ]
            , onClick (SelectTangen tangen)
            ]

        firstChapterRow =
            case List.head tangen.chapters of
                Nothing ->
                    [ td [] [ text "" ], td [] [ text "" ] ]

                Just chapter ->
                    viewChapterRowData selectedChapterIds chapter
    in
    tr [] <|
        [ th tangenClasses [ text (String.fromInt tangen.number) ]
        , td tangenClasses [ text tangen.title ]
        ]
            ++ firstChapterRow


viewTangenChapterRowRest : Set Int -> Tangen -> List (Html Msg)
viewTangenChapterRowRest selectedChapterIds tangen =
    List.drop 1 tangen.chapters
        |> List.map (viewChapterRowData selectedChapterIds)
        |> List.map (tr [])


viewChapterRowData : Set Int -> Chapter -> List (Html Msg)
viewChapterRowData selectedChapterIds chapter =
    let
        isChapterSelected =
            Set.member chapter.id selectedChapterIds

        textChpNum =
            "第" ++ String.fromInt chapter.number ++ "章 " ++ chapter.title
    in
    [ td
        [ classList [ ( "is-info", isChapterSelected ) ]
        , onClick (SelectChapter chapter)
        ]
        [ text textChpNum ]
    , td [ class "has-text-centered" ]
        [ label [ class "checkbox" ]
            [ input
                [ type_ "checkbox"
                , onClick (SelectChapter chapter)
                , checked isChapterSelected
                ]
                []
            ]
        ]
    ]


viewBookFilterControl : BookFilter.State -> Html Msg
viewBookFilterControl filters =
    nav [ class "level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ]
                [ div [ class "field is-grouped" ] <|
                    List.map
                        (viewBookTargetFilterButton filters.targetFilter)
                        [ "小学生以下", "中学・高校", "大学・一般" ]
                ]
            , viewBookCollFilterButtons filters.collFilter
            , div [ class "level-item" ] [ viewKyCaseFilterButton filters.kyCaseFilterOn ]
            ]
        , div [ class "level-right" ]
            [ div [ class "level-item" ]
                [ div [ class "field has-addons" ]
                    [ p [ class "control" ] <| viewFilterQueryModeButtons filters.queryFilterMode
                    , p [ class "control" ]
                        [ p [ class "control" ]
                            [ input
                                [ class "input is-small"
                                , type_ "text"
                                , placeholder "書誌情報に …… を含む"
                                , value filters.queryFilterStr
                                , onInput (BookFilterMsg << EditQueryFilter)
                                ]
                                []
                            ]
                        ]
                    , p [ class "control" ]
                        [ button
                            [ classList
                                [ ( "button is-small", True )
                                , ( "is-danger is-outlined", filters.queryFilterOn )
                                ]
                            , onClick (BookFilterMsg <| ToggleQueryFilter (not filters.queryFilterOn))
                            ]
                            [ text <|
                                if filters.queryFilterOn then
                                    "絞込解除"

                                else
                                    "絞り込む"
                            ]
                        ]
                    ]
                ]
            ]
        ]


targetTagColourHelper : Set String -> String -> Html.Attribute Msg
targetTagColourHelper targetFilter bookTarget =
    classList
        [ ( "is-info", bookTarget == "小学生以下" )
        , ( "is-warning", bookTarget == "中学・高校" )
        , ( "is-dark", bookTarget == "大学・一般" && Set.member "大学・一般" targetFilter )
        , ( "is-light", not (Set.member bookTarget targetFilter) )
        ]


viewBookTargetFilterButton : Set String -> String -> Html Msg
viewBookTargetFilterButton targetFilter bookTarget =
    button
        [ class "button is-small level-item"
        , targetTagColourHelper targetFilter bookTarget
        , onClick (BookFilterMsg <| SetBookTargetFilter bookTarget)
        ]
        [ text bookTarget ]


viewBookCollFilterButtons : CollFilter -> Html Msg
viewBookCollFilterButtons collFilter =
    div [ class "level-item" ]
        [ div [ class "field has-addons" ]
            [ div [ class "control" ] <|
                List.map (viewBookCollFilterButton collFilter) listAllCF
            ]
        ]


viewBookCollFilterButton : CollFilter -> CollFilter -> Html Msg
viewBookCollFilterButton currentCollFilter collFilterToShow =
    button
        [ class "button is-small"
        , classList [ ( "is-active", currentCollFilter == collFilterToShow ) ]
        , onClick (BookFilterMsg <| SetBookCollFilter collFilterToShow)
        ]
        [ text <| cfToString collFilterToShow ]


viewKyCaseFilterButton : Bool -> Html Msg
viewKyCaseFilterButton kyCaseFilterOn =
    button
        [ class "button is-small level-item"
        , classList [ ( "is-active", kyCaseFilterOn ) ]
        , onClick (BookFilterMsg <| ToggleKyCaseFilter (not kyCaseFilterOn))
        ]
        [ span [] [ text "活用DB情報あり" ]
        , span [ class "icon is-small" ] [ i [ class "fas fa-database" ] [] ]
        ]


viewFilterQueryModeButtons : QueryMode -> List (Html Msg)
viewFilterQueryModeButtons queryMode =
    let
        viewFQMButton mode =
            button
                [ classList
                    [ ( "button is-small is-light", True )
                    , ( "is-active", queryMode == mode )
                    ]
                , onClick (BookFilterMsg <| SetQueryFilterMode mode)
                ]
                [ text <| qmToString mode ]
    in
    List.map viewFQMButton listAllQM


viewBookBrowser : Model -> List (Html Msg)
viewBookBrowser model =
    let
        filteredBookDict : Dict String (List Book)
        filteredBookDict =
            case model.bookStore of
                Loaded perNdcBookDict ->
                    filterBooksForAllNdc model.filters perNdcBookDict

                _ ->
                    Dict.empty

        filteredNdcCntDict : Dict String Int
        filteredNdcCntDict =
            countBooksPerNdc filteredBookDict

        paginate bookList =
            List.drop (12 * (model.selectedPagination - 1)) bookList
                |> List.take 12

        paginatedBooksOfNdc =
            Dict.get model.selectedNdcTab filteredBookDict
                |> Maybe.withDefault []
                |> paginate
    in
    [ div [ class "tabs is-boxed" ]
        [ ul [] <|
            List.map
                (viewNdcTab model filteredNdcCntDict)
                (mapSortedNdcCountList model.bookStore)
                ++ viewNewTab model.bookStore
        ]
    , viewPagination 12 model.selectedNdcTab filteredNdcCntDict model.selectedPagination
    , Keyed.node "div"
        [ class "columns is-multiline is-mobile has-background-white-bis"
        , style "min-height" "40vh"

        -- , style "overflow-y" "auto"  -- is-vcentered
        ]
        (List.map (viewKeyedBook model.filters model.selectedBookIds) paginatedBooksOfNdc)
    ]


viewNewTab : BookStore -> List (Html Msg)
viewNewTab bookStore =
    case bookStore of
        Loaded _ ->
            [ li [ onClick ToggleAdvSearchModal ]
                [ a [ class "icon is-large" ] [ i [ class "fas fa-lg fa-plus" ] [] ] ]
            ]

        _ ->
            []


viewNdcTab : Model -> Dict String Int -> ( String, Int ) -> Html Msg
viewNdcTab { filters, ndcLabelStore, selectedNdcTab } ndcCntDict ( ndc, cnt ) =
    let
        ndcLabel =
            lookupNdcLabel ndc ndcLabelStore

        textNdc =
            [ strong [] [ text <| ndc ++ " " ++ ndcLabel ]
            , span [] [ text "(" ]
            ]

        textCnt =
            [ span [] [ text (String.fromInt cnt) ]
            , span [] [ text "冊)" ]
            ]
    in
    li
        [ classList [ ( "is-active", selectedNdcTab == ndc ) ]
        , onClick (SelectNdcTab ndc)
        ]
        [ a [] <|
            case Dict.get ndc ndcCntDict of
                Just filteredCnt ->
                    if isFiltered filters then
                        textNdc
                            ++ [ span
                                    [ class "has-text-danger" ]
                                    [ text (String.fromInt filteredCnt) ]
                               , span [] [ text "/" ]
                               ]
                            ++ textCnt

                    else
                        textNdc ++ textCnt

                Nothing ->
                    []
        ]


viewPagination : Int -> String -> Dict String Int -> Int -> Html Msg
viewPagination booksPerPage selectedNdc ndcCntDict selectedPagination =
    let
        totalCnt =
            Maybe.withDefault 0 <| Dict.get selectedNdc ndcCntDict

        divided =
            totalCnt // booksPerPage

        remainder =
            remainderBy booksPerPage totalCnt

        numPages =
            if remainder > 0 then
                divided + 1

            else
                divided

        pLink i =
            li
                [ class "pagination-link"
                , classList [ ( "is-current", i == selectedPagination ) ]
                , onClick (SelectedPagination i)
                ]
                [ text <| String.fromInt i ]
    in
    nav [ class "pagination is-centered is-small" ]
        [ button
            [ class "button pagination-previous"
            , onClick (SelectedPagination <| max 1 (selectedPagination - 1))
            , disabled (selectedPagination == 1)
            ]
            [ text "←" ]
        , button
            [ class "button pagination-next"
            , onClick (SelectedPagination <| min numPages (selectedPagination + 1))
            , disabled (selectedPagination == numPages || numPages == 0)
            ]
            [ text "→" ]
        , ul [ class "pagination-list" ] <|
            if selectedNdc == "" then
                []

            else if numPages < 1 then
                [ pLink 1 ]

            else if numPages <= 20 then
                List.map pLink (List.range 1 numPages)

            else if selectedPagination == 9 then
                List.concat
                    [ List.map pLink (List.range 1 8)
                    , [ li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      , pLink 9
                      , pLink 10
                      , pLink 11
                      , li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      ]
                    , List.map pLink (List.range (numPages - 8) numPages)
                    ]

            else if selectedPagination > 8 && selectedPagination < (numPages - 9) then
                -- FIXME: 8 and numPages - 8 duplicates at the middle when selectedPagination == 8 or numPages - 8
                List.concat
                    [ List.map pLink (List.range 1 8)
                    , [ li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      , pLink (selectedPagination - 1)
                      , pLink selectedPagination
                      , pLink (selectedPagination + 1)
                      , li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      ]
                    , List.map pLink (List.range (numPages - 8) numPages)
                    ]

            else if selectedPagination == (numPages - 9) then
                List.concat
                    [ List.map pLink (List.range 1 8)
                    , [ li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      , pLink (numPages - 11)
                      , pLink (numPages - 10)
                      , pLink (numPages - 9)
                      , li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      ]
                    , List.map pLink (List.range (numPages - 8) numPages)
                    ]

            else
                let
                    midPage =
                        numPages // 2
                in
                List.concat
                    [ List.map pLink (List.range 1 8)
                    , [ li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      , pLink (midPage - 1)
                      , pLink midPage
                      , pLink (midPage + 1)
                      , li [] [ span [ class "pagination-ellipsis" ] [ text "…" ] ]
                      ]
                    , List.map pLink (List.range (numPages - 8) numPages)
                    ]
        ]


filterBooksForAllNdc : BookFilter.State -> Dict String (List Book) -> Dict String (List Book)
filterBooksForAllNdc filters perNdcBookDict =
    Dict.map
        (\_ bookList -> filterBooks filters bookList)
        perNdcBookDict


filterBooks : BookFilter.State -> List Book -> List Book
filterBooks filters bookList =
    bookList
        |> filterBooksByQuery filters.queryFilterOn filters.queryFilterMode filters.queryFilterStr
        |> filterBooksByBookTarget filters.targetFilter
        |> filterBooksByBookColl filters.collFilter
        |> filterBooksWithKyCases filters.kyCaseFilterOn


filterBooksWithKyCases : Bool -> List Book -> List Book
filterBooksWithKyCases flag bookList =
    if flag then
        List.filter (\book -> not (List.isEmpty book.caseIds)) bookList

    else
        bookList


filterBooksByQuery : Bool -> QueryMode -> String -> List Book -> List Book
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
        , book.subtitle
        , String.join " " book.authors
        , String.join " " book.publishers
        , String.join " " book.topics
        , String.join " " book.notes
        , book.toc
        , book.libcom
        ]


filterBooksByBookTarget : Set String -> List Book -> List Book
filterBooksByBookTarget targetFilter books =
    let
        setBookTargets : Book -> Set String
        setBookTargets book =
            Set.fromList (String.split "|" book.target)

        matchBookTargetFilter : Book -> Set String
        matchBookTargetFilter book =
            Set.intersect targetFilter (setBookTargets book)
    in
    if Set.isEmpty targetFilter then
        books

    else
        List.filter
            (\book -> not (Set.isEmpty <| matchBookTargetFilter book))
            books


filterBooksByBookColl : CollFilter -> List Book -> List Book
filterBooksByBookColl collFilter books =
    case collFilter of
        Own ->
            List.filter .local books

        ILL ->
            List.filter (not << .local) books

        Both ->
            books


viewKeyedBook : BookFilter.State -> Set Int -> Book -> ( String, Html Msg )
viewKeyedBook filters selectedBookIds book =
    ( String.fromInt book.id, lazy3 viewBook filters selectedBookIds book )


viewBook : BookFilter.State -> Set Int -> Book -> Html Msg
viewBook filters selectedBookIds book =
    div
        [ id "book"
        , class "column is-2 has-text-left"
        , classList [ ( "has-background-info", Set.member book.id selectedBookIds ) ]
        ]
        [ label [ class "checkbox" ]
            [ input
                [ type_ "checkbox"
                , onClick (SelectBook book)
                , checked (Set.member book.id selectedBookIds)
                ]
                []
            , strong
                [ classList
                    [ ( "is-size-6", True )
                    , ( "has-text-white", Set.member book.id selectedBookIds )
                    ]
                ]
                (highlightQueries filters book.title)
            , if List.isEmpty book.caseIds then
                span [] []

              else
                span [ class "icon is-small" ] [ i [ class "fas fa-database" ] [] ]
            , viewBookTargetTags filters.targetFilter book.target
            ]
        , figure
            [ class "image"
            , onClick (ToggleBookModal (Opened book))
            ]
            (viewBookCoverImg book)
        ]


viewBookCoverImg : Book -> List (Html Msg)
viewBookCoverImg book =
    if book.cover == "" then
        [ strong
            [ style "position" "absolute"
            , style "top" "10%"
            , style "left" "10%"
            , style "width" "80%"
            , style "font-size" "1.5em"
            , style "color" "white"
            , style "text-align" "center"
            ]
            [ text <|
                if String.length book.title > 20 then
                    String.left 19 book.title ++ "…"

                else
                    book.title
            ]
        , img [ src "/assets/blank_cover.png" ] []
        ]

    else
        [ img [ src book.cover ] [] ]


viewBookTargetTags : Set String -> String -> Html Msg
viewBookTargetTags targetFilter target =
    let
        viewTargetTag t =
            span
                [ class "tag"
                , targetTagColourHelper targetFilter t
                ]
                [ text t ]
    in
    if target == "" then
        div [] []

    else
        div [ class "tags" ] <|
            List.map viewTargetTag (String.split "|" target)


viewBookModal : Model -> Html Msg
viewBookModal { filters, selectedBookIds, bookModal, kyCases } =
    let
        highlighter : String -> List (Html Msg)
        highlighter =
            highlightQueries filters

        targetFilter =
            filters.targetFilter

        bookIsSelected =
            Set.member book.id selectedBookIds

        book =
            case bookModal of
                Opened book_ ->
                    book_

                Closed ->
                    nullBook
    in
    div
        [ class "modal"
        , classList [ ( "is-active", bookModal /= Closed ) ]
        ]
        [ div
            [ class "modal-background"
            , onClick (ToggleBookModal Closed)
            ]
            []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ strong [] <| highlighter book.title ]
                , button
                    [ attribute "aria-label" "close"
                    , class "delete"
                    , onClick (ToggleBookModal Closed)
                    ]
                    []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "tile is-ancestor is-vertical" ]
                    [ div [ class "tile" ]
                        [ div [ class "tile is-vertical is-4 is-parent" ]
                            [ div [ class "tile is-child" ]
                                [ figure [ class "image" ] (viewBookCoverImg book)
                                , p [ class "buttons" ]
                                    [ a
                                        [ class "button is-small is-warning"
                                        , disabled (book.isbn10 == "")
                                        , href ("https://www.amazon.co.jp/dp/" ++ String.replace "-" "" book.isbn10)
                                        , target "_blank"
                                        ]
                                        [ span [ class "icon" ]
                                            [ i [ class "fab fa-amazon" ] [] ]
                                        , span [] [ text "Amazon" ]
                                        ]
                                    ]
                                , strong [ class "is-6" ] [ text "キーワード" ]
                                , p [ class "is-size-7" ] <|
                                    highlighter (String.join "・" book.topics)
                                ]
                            ]
                        , div [ class "tile is-vertical is-parent" ] <|
                            [ div [ class "tile is-child" ]
                                [ strong [ class "is-6" ] [ text "図書詳細" ]
                                , table [ class "table is-size-7 is-narrow is-fullwidth" ]
                                    [ tr []
                                        [ th [] [ text "副題" ]
                                        , td [] <| highlighter book.subtitle
                                        ]
                                    , tr []
                                        [ th [] [ text "出版年" ]
                                        , td [] <| highlighter (String.fromInt book.year ++ "年")
                                        ]
                                    , tr []
                                        [ th [] [ text "著者" ]
                                        , td [] <| highlighter (String.join "・" book.authors)
                                        ]
                                    , tr []
                                        [ th [] [ text "出版社" ]
                                        , td [] <| highlighter (String.join "・" book.publishers)
                                        ]
                                    , tr []
                                        [ th [] [ text "総ページ数" ]
                                        , td [] [ text book.pages ]
                                        ]
                                    , tr []
                                        [ th [] [ text "NDC" ]
                                        , td [] [ text book.ndc_full ]
                                        ]
                                    , tr []
                                        [ th [] [ text "ISBN" ]
                                        , td [] [ text book.isbn13 ]
                                        ]
                                    , tr []
                                        [ th [] [ text "対象学年" ]
                                        , td [] [ viewBookTargetTags targetFilter book.target ]
                                        ]
                                    ]
                                ]
                            , div [ class "tile is-child" ] <|
                                strong [ class "is-6" ] [ text "内容紹介" ]
                                    :: List.map
                                        (\note ->
                                            p [ class "is-small" ] <| highlighter note
                                        )
                                        book.notes
                            , viewToC highlighter book
                            , viewLibCom highlighter book
                            ]
                        ]
                    , viewKatsuyouDBdata kyCases book
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button
                    [ classList
                        [ ( "button", True )
                        , ( "is-warning", bookIsSelected )
                        , ( "is-success", not bookIsSelected )
                        ]
                    , onClick (SelectBook book)
                    ]
                    [ text
                        (if bookIsSelected then
                            "選択解除"

                         else
                            "選択"
                        )
                    ]
                , button
                    [ class "button"
                    , onClick (ToggleBookModal Closed)
                    ]
                    [ text "教材候補に戻る" ]
                ]
            ]
        ]


viewToC : (String -> List (Html Msg)) -> Book -> Html Msg
viewToC highlighter book =
    if book.toc /= "" then
        div [ class "tile is-child" ]
            [ strong [ class "is-6" ] [ text "目次" ]
            , ul [ class "is-small" ] <|
                List.map (\t -> li [] (highlighter t)) (String.split "\n" book.toc)
            ]

    else
        div [] []


viewLibCom : (String -> List (Html Msg)) -> Book -> Html Msg
viewLibCom highlighter book =
    if book.libcom /= "" then
        div [ class "tile is-child" ]
            [ strong [ class "is-6" ] [ text "教員・司書のコメント" ]
            , p [ class "is-small" ] <| highlighter book.libcom
            ]

    else
        div [] []


viewKatsuyouDBdata : List KyCase -> Book -> Html Msg
viewKatsuyouDBdata kyCases book =
    if List.isEmpty book.caseIds then
        div [] []

    else
        div [ class "tile is-parent" ]
            [ div [ class "tile is-child" ]
                [ strong [ class "is-6" ] [ text "活用DB情報" ]
                , table [ class "table is-size-7 is-narrow" ]
                    [ thead [ class "has-text-left" ]
                        [ tr []
                            [ th [] []
                            , th [] [ text "目的" ]
                            , th [] [ text "校種" ]
                            , th [] [ text "教科" ]
                            , th [] [ text "対象学年" ]
                            , th [] [ text "単元" ]
                            , th [] [ text "実施日" ]
                            ]
                        ]
                    , tbody [] (viewKyCases kyCases)
                    ]
                ]
            ]


viewKyCases : List KyCase -> List (Html Msg)
viewKyCases kyCases =
    let
        viewKyCase : KyCase -> Html Msg
        viewKyCase kyCase =
            tr []
                [ td []
                    [ a [ class "tag is-link", href kyCase.url, target "_blank" ]
                        [ text kyCase.caseident ]
                    ]
                , td [] [ text <| String.left 30 kyCase.purpose ++ "…" ]
                , td [] [ text kyCase.school ]
                , td [] [ text kyCase.subject ]
                , td [] [ text kyCase.target ]
                , td [] [ text kyCase.tangen ]
                , td [] [ text kyCase.date ]
                ]
    in
    List.map viewKyCase kyCases


viewAdvSearchModal : Model -> Html Msg
viewAdvSearchModal { advSearchModalOpened, advSearchQuery, advSearchNdc, advSearchStatus, ndcLabelStore } =
    div
        [ class "modal"
        , classList [ ( "is-active", advSearchModalOpened == True ) ]
        ]
        [ div
            [ class "modal-background"
            , onClick ToggleAdvSearchModal
            ]
            []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ strong [] [ text "詳細検索結果をタブに追加" ] ]
                , button
                    [ attribute "aria-label" "close"
                    , class "delete"
                    , onClick ToggleAdvSearchModal
                    ]
                    []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "キーワード" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ p [ class "control is-expanded" ]
                                [ input
                                    [ class "input"
                                    , type_ "text"
                                    , placeholder "全文検索したいキーワードを入力"
                                    , value advSearchQuery
                                    , onInput EditAdvSearchQuery
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "NDC" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field is-narrow" ]
                            [ div [ class "control" ]
                                [ div [ class "select is-fullwidth" ]
                                    [ select [ onInput SelectAdvSearchNdc ] <|
                                        option [ value "", selected (advSearchNdc == "") ] [ text "指定しない" ]
                                            :: viewNdcLabelOptions advSearchNdc ndcLabelStore
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button
                    [ class <|
                        "button "
                            ++ (case advSearchStatus of
                                    Fetching ->
                                        "is-success is-loading"

                                    FetchErr ->
                                        "is-danger"

                                    _ ->
                                        "is-success"
                               )
                    , onClick RequestAdvSearch
                    , disabled (advSearchQuery == "" && advSearchNdc == "")
                    ]
                    [ text <|
                        case advSearchStatus of
                            FetchErr ->
                                "結果0件または通信失敗（リトライ）"

                            _ ->
                                "検索"
                    ]
                , button
                    [ class "button"
                    , onClick ToggleAdvSearchModal
                    ]
                    [ text "キャンセル" ]
                ]
            ]
        ]


viewNdcLabelOptions : String -> NdcLabelStore -> List (Html Msg)
viewNdcLabelOptions advSearchNdc ndcLabelStore =
    case ndcLabelStore of
        Loaded ndcLabelDict ->
            ndcLabelDict
                |> Dict.toList
                |> List.map (viewNdcLabelOption advSearchNdc)

        _ ->
            []


viewNdcLabelOption : String -> ( String, String ) -> Html Msg
viewNdcLabelOption advSearchNdc ( ndc, label ) =
    option [ value ndc, selected (advSearchNdc == ndc) ] [ text <| ndc ++ ": " ++ label ]


highlightQueries : BookFilter.State -> String -> List (Html Msg)
highlightQueries { queryFilterOn, queryFilterStr } txt =
    let
        queries =
            String.split " " <| String.replace "\u{3000}" " " queryFilterStr

        viewHighlightToken : String -> Html Msg
        viewHighlightToken token =
            if String.startsWith "__" token then
                span
                    [ class "has-background-warning has-text-black" ]
                    [ text <| String.dropLeft 2 token ]

            else
                span [] [ text token ]
    in
    if queryFilterOn then
        highlightQueriesHelper queries [ txt ]
            |> List.map viewHighlightToken

    else
        [ text txt ]


highlightQueriesHelper : List String -> List String -> List String
highlightQueriesHelper queries hlTexts =
    case queries of
        [] ->
            hlTexts

        q :: restQ ->
            highlightQueriesHelper restQ (highlightQuery q hlTexts)


highlightQuery : String -> List String -> List String
highlightQuery query hlTexts =
    let
        markHlTokens : String -> List String
        markHlTokens txt =
            if String.startsWith "__" txt then
                [ txt ]

            else
                List.intersperse ("__" ++ query) (String.split query txt)
    in
    hlTexts
        |> List.map markHlTokens
        |> List.concat


viewTableOutput : Set Int -> BookStore -> Html Msg
viewTableOutput selectedBookIds bookStore =
    let
        noBooks =
            Set.isEmpty selectedBookIds
    in
    section [ class "section" ]
        [ div [ class "container" ]
            [ h2 [ class "title no-print" ]
                [ text "図書リスト"
                , span [ class "tag is-rounded" ]
                    [ text <| (String.fromInt << Set.size) selectedBookIds ++ "冊" ]
                ]
            , div [ class "columns" ]
                [ div [ class "column is-full" ]
                    [ table [ class "table is-fullwidth is-hoverable" ]
                        [ thead [ class "has-text-left" ]
                            [ tr []
                                [ th [] []
                                , th [] []
                                , th [] [ text "タイトル" ]
                                , th [] [ text "著者" ]
                                , th [] [ text "出版年" ]
                                , th [] [ text "出版社" ]
                                , th [] [ text "NDC" ]
                                ]
                            ]
                        , tbody [] (viewTableOutputRows selectedBookIds bookStore)
                        ]
                    , div [ class "buttons no-print" ]
                        [ button
                            [ class "button is-info"
                            , onClick RequestPrint
                            , disabled noBooks
                            ]
                            [ text "印刷" ]
                        , button
                            [ class "button is-warning"
                            , onClick RequestCsv
                            , disabled noBooks
                            ]
                            [ text "CSVファイルで出力" ]
                        , button
                            [ class "button is-warning"
                            , onClick RequestTsv
                            , disabled noBooks
                            ]
                            [ text "タブ区切りファイルで出力" ]
                        , p [ class "help" ]
                            [ text "ファイル出力はUTF-8エンコーディングされます（参考："
                            , a [ href "https://www.ipentec.com/document/office-excel-open-utf-8-csv-file", target "_blank" ]
                                [ text "Excel での開き方" ]
                            , text "）"
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewTableOutputRows : Set Int -> BookStore -> List (Html Msg)
viewTableOutputRows selectedBookIds bookStore =
    listSelectedBooks selectedBookIds bookStore
        |> List.map viewTableOutputRow


viewTableOutputRow : Book -> Html Msg
viewTableOutputRow book =
    let
        openModal =
            onClick (ToggleBookModal (Opened book))

        bookThumbnail =
            [ img
                [ style "width" "60px"
                , style "height" "80px"
                , src <|
                    if book.cover == "" then
                        "/assets/blank_cover.png"

                    else
                        book.cover
                ]
                []
            ]
    in
    tr []
        [ td [ class "has-text-centered" ]
            [ label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , onClick (SelectBook book)
                    , checked True
                    ]
                    []
                ]
            ]
        , td [ openModal ] bookThumbnail
        , td [ openModal ] [ strong [] [ text book.title ] ]
        , td [ openModal ] [ text (String.join "・" book.authors) ]
        , td [ openModal ] [ text (String.fromInt book.year ++ "年") ]
        , td [ openModal ] [ text (String.join "・" book.publishers) ]
        , td [ openModal ] [ text book.ndc ]
        ]
