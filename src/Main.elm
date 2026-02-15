port module Main exposing (..)

import Api exposing (..)
import BookFilter
import Browser
import Csv.Encode as CsvEn
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3, lazy4)
import Http
import Json.Decode
import NdcSelect
import School exposing (School(..))
import Set exposing (Set)
import Utils exposing (LocalStore(..), onClickSP, toggleId, toggleSetMember)



-- PORTS


port saveToLocalStorage : { key : String, value : String } -> Cmd msg


port removeFromLocalStorage : String -> Cmd msg


port requestPrint : Bool -> Cmd msg


port modalState : Bool -> Cmd msg


port requestUnitradByNdc : ( String, String ) -> Cmd msg


port receiveUnitradByNdc : (Json.Decode.Value -> msg) -> Sub msg


port requestMapping : String -> Cmd msg


port receiveMapping : (Json.Decode.Value -> msg) -> Sub msg



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveUnitradByNdc (GotBooksOfNdc9 << Json.Decode.decodeValue unitradResultDecoder)
        , receiveMapping (GotMapping << Json.Decode.decodeValue unitradMappingDecoder)
        ]



-- MODEL HELPERS


type BookModal
    = Closed
    | Opened Book


type BookDisplayMode
    = ThumbnailMode
    | TableMode


type CoverSize
    = SmallCover
    | LargeCover


coverSizeToString : CoverSize -> String
coverSizeToString cs =
    case cs of
        SmallCover ->
            "small"

        LargeCover ->
            "large"


stringToCoverSize : String -> CoverSize
stringToCoverSize s =
    case s of
        "small" ->
            SmallCover

        _ ->
            LargeCover


type Stage
    = PrefectureSelectingStage
    | NdcSelectingStage
    | ExploringStage


type alias Ndc9LabelStore =
    LocalStore (Dict String String)


type alias BookCache =
    Dict String UnitradResult



-- MODEL


type alias Flags =
    { prefecture : String
    , coverSize : String
    }


type alias Model =
    { stage : Stage

    -- Prefecture
    , prefectures : LocalStore (List Prefecture)
    , selectedPrefecture : Maybe Prefecture

    -- NdcSelect
    , ndcSelect : NdcSelect.Model

    -- NDC labels
    , ndc9LabelStore : Ndc9LabelStore

    -- Book browser
    , bookCache : BookCache
    , bookFetchErrCount : Int
    , selectedBookIds : Set String
    , selectedNdcTab : Maybe String
    , selectedPagination : Int
    , bookDisplayMode : BookDisplayMode
    , coverSize : CoverSize
    , bookListOpened : Bool
    , bookModal : BookModal

    -- Mapping
    , mappingStatus : MappingStatus

    -- Extra NDC
    , extraNdc9ModalOpened : Bool
    , predNdc9Query : String
    , predNdc9Store : LocalStore (List PredNdc9)
    , extraNdc9Direct : String
    , selectedExtraNdc9s : Set String

    -- Filters
    , filters : BookFilter.State
    }


initModel : Model
initModel =
    { stage = PrefectureSelectingStage
    , prefectures = Loading
    , selectedPrefecture = Nothing
    , ndcSelect = NdcSelect.initModel
    , ndc9LabelStore = NotLoaded
    , bookCache = Dict.empty
    , bookFetchErrCount = 0
    , selectedBookIds = Set.empty
    , selectedNdcTab = Nothing
    , selectedPagination = 1
    , bookDisplayMode = ThumbnailMode
    , coverSize = LargeCover
    , bookListOpened = False
    , bookModal = Closed
    , mappingStatus = MappingNotRequested
    , extraNdc9ModalOpened = False
    , predNdc9Query = ""
    , predNdc9Store = NotLoaded
    , extraNdc9Direct = ""
    , selectedExtraNdc9s = Set.empty
    , filters = BookFilter.default
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initModel | coverSize = stringToCoverSize flags.coverSize }
    , Cmd.batch
        [ loadPrefectures (GotPrefectures flags.prefecture)
        , loadNdc9Labels GotNdc9Labels
        ]
    )



-- UPDATE


type Msg
    = -- Data loading
      GotPrefectures String (Result Http.Error (List Prefecture))
    | GotNdc9Labels (Result Http.Error (List Ndc9))
      -- Prefecture selection
    | SelectPrefecture Prefecture
    | ChangePrefecture
      -- NdcSelect
    | NdcSelectMsg NdcSelect.Msg
      -- Stage transition
    | FetchBooks
    | GotBooksOfNdc9 (Result Json.Decode.Error UnitradResult)
    | GotMapping (Result Json.Decode.Error UnitradMapping)
      -- Book browser
    | SelectNdcTab String
    | CloseNdc9Tab String
    | SelectPagination Int
    | ChangeBookDisplayMode BookDisplayMode
    | SetCoverSize CoverSize
    | ToggleBookModal BookModal
    | SelectBook Book
    | ToggleBookList
      -- Extra NDC
    | ToggleExtraNdc9Modal
    | EditPredNdc9Query String
    | PredictNdc9s
    | GotPredNdc9s (Result Http.Error (List PredNdc9))
    | EditExtraNdc9Direct String
    | AddExtraNdc9Direct
    | ToggleExtraNdc9 String
    | FetchBooksOfExtraNdc9
      -- Filters
    | BookFilterMsg BookFilter.Msg
      -- Export
    | RequestTsv
    | RequestCsv
    | RequestPrint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Data loading
        GotPrefectures savedKey result ->
            case result of
                Ok prefectures ->
                    let
                        savedPref =
                            if savedKey /= "" then
                                List.filter (\p -> p.key == savedKey) prefectures
                                    |> List.head

                            else
                                Nothing
                    in
                    case savedPref of
                        Just pref ->
                            ( { model
                                | prefectures = Loaded prefectures
                                , selectedPrefecture = Just pref
                                , stage = NdcSelectingStage
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | prefectures = Loaded prefectures }, Cmd.none )

                Err _ ->
                    ( { model | prefectures = LoadErr }, Cmd.none )

        GotNdc9Labels result ->
            case result of
                Ok ndc9s ->
                    let
                        ndc9LabelDict =
                            Dict.fromList <| List.map (\x -> ( x.symbol, x.label )) ndc9s
                    in
                    ( { model | ndc9LabelStore = Loaded ndc9LabelDict }, Cmd.none )

                Err _ ->
                    ( { model | ndc9LabelStore = LoadErr }, Cmd.none )

        -- Prefecture selection
        SelectPrefecture pref ->
            ( { model
                | selectedPrefecture = Just pref
                , stage = NdcSelectingStage
              }
            , saveToLocalStorage { key = "prefecture", value = pref.key }
            )

        ChangePrefecture ->
            ( { initModel
                | prefectures = model.prefectures
                , ndc9LabelStore = model.ndc9LabelStore
                , coverSize = model.coverSize
              }
            , removeFromLocalStorage "prefecture"
            )

        -- NdcSelect
        NdcSelectMsg subMsg ->
            case model.stage of
                NdcSelectingStage ->
                    let
                        ( newSubModel, cmds ) =
                            NdcSelect.update subMsg model.ndcSelect
                    in
                    ( { model | ndcSelect = newSubModel }, Cmd.map NdcSelectMsg cmds )

                _ ->
                    ( model, Cmd.none )

        -- Stage transition
        FetchBooks ->
            case model.selectedPrefecture of
                Nothing ->
                    ( model, Cmd.none )

                Just pref ->
                    let
                        ndc9sToFetch =
                            NdcSelect.toSelectedNdc9Symbols model.ndcSelect

                        newBookCache =
                            ndc9sToFetch
                                |> List.map (\ndc -> ( ndc, urInit ndc ))
                                |> Dict.fromList

                        cmdsInitSearch =
                            initSearchBooks pref.key ndc9sToFetch

                        needsMapping =
                            model.mappingStatus == MappingNotRequested

                        mappingCmd =
                            if needsMapping then
                                requestMapping pref.key

                            else
                                Cmd.none
                    in
                    ( { model
                        | stage = ExploringStage
                        , bookCache = newBookCache
                        , bookFetchErrCount = 0
                        , selectedBookIds = Set.empty
                        , selectedNdcTab = Nothing
                        , selectedPagination = 1
                        , bookListOpened = False
                        , mappingStatus =
                            if needsMapping then
                                MappingRequested

                            else
                                model.mappingStatus
                      }
                    , Cmd.batch (mappingCmd :: cmdsInitSearch)
                    )

        GotBooksOfNdc9 result ->
            case result of
                Ok ur ->
                    addUrToBookCache ur model

                Err _ ->
                    incrementBookFetchError model

        GotMapping result ->
            case result of
                Ok um ->
                    let
                        normalised =
                            case model.selectedPrefecture of
                                Just pref ->
                                    normaliseUM pref.primaryLibId um

                                Nothing ->
                                    um
                    in
                    ( { model | mappingStatus = MappingReceived normalised }, Cmd.none )

                Err _ ->
                    ( { model | mappingStatus = MappingError }, Cmd.none )

        -- Book browser
        SelectNdcTab ndc ->
            ( { model | selectedNdcTab = Just ndc, selectedPagination = 1 }, Cmd.none )

        CloseNdc9Tab ndc ->
            case Dict.get ndc model.bookCache of
                Nothing ->
                    ( model, Cmd.none )

                Just ur ->
                    let
                        newBookCache =
                            Dict.remove ndc model.bookCache

                        firstNdc9 =
                            List.head <| Dict.keys newBookCache

                        newSelectedBookIds =
                            Set.diff model.selectedBookIds (Set.fromList <| List.map .id ur.books)
                    in
                    ( { model
                        | selectedNdcTab = firstNdc9
                        , selectedPagination = 1
                        , selectedBookIds = newSelectedBookIds
                        , bookCache = newBookCache
                      }
                    , Cmd.none
                    )

        SelectPagination numPage ->
            ( { model | selectedPagination = numPage }, Cmd.none )

        ChangeBookDisplayMode bdm ->
            ( { model | bookDisplayMode = bdm, selectedPagination = 1 }, Cmd.none )

        SetCoverSize newCoverSize ->
            ( { model | coverSize = newCoverSize, selectedPagination = 1 }
            , saveToLocalStorage { key = "explorerCoverSize", value = coverSizeToString newCoverSize }
            )

        ToggleBookModal bookModal ->
            ( { model | bookModal = bookModal }
            , case bookModal of
                Opened _ ->
                    modalState True

                Closed ->
                    modalState False
            )

        SelectBook book ->
            case model.bookModal of
                Opened _ ->
                    ( { model
                        | selectedBookIds = toggleId book model.selectedBookIds
                        , bookModal = Closed
                      }
                    , modalState False
                    )

                Closed ->
                    ( { model | selectedBookIds = toggleId book model.selectedBookIds }
                    , Cmd.none
                    )

        ToggleBookList ->
            ( { model | bookListOpened = not model.bookListOpened }, Cmd.none )

        -- Extra NDC
        ToggleExtraNdc9Modal ->
            ( { model
                | extraNdc9ModalOpened = not model.extraNdc9ModalOpened
                , predNdc9Query = ""
                , predNdc9Store = NotLoaded
                , selectedExtraNdc9s = Set.empty
              }
            , modalState (not model.extraNdc9ModalOpened)
            )

        EditPredNdc9Query query ->
            ( { model | predNdc9Query = query }, Cmd.none )

        PredictNdc9s ->
            ( { model | predNdc9Store = Loading }
            , getPredNdc9s GotPredNdc9s model.predNdc9Query
            )

        GotPredNdc9s result ->
            case result of
                Ok predNdc9s ->
                    ( { model | predNdc9Store = Loaded predNdc9s }, Cmd.none )

                Err _ ->
                    ( { model | predNdc9Store = LoadErr }, Cmd.none )

        EditExtraNdc9Direct ndc9 ->
            ( { model | extraNdc9Direct = ndc9 }, Cmd.none )

        AddExtraNdc9Direct ->
            ( { model
                | selectedExtraNdc9s = Set.insert model.extraNdc9Direct model.selectedExtraNdc9s
                , extraNdc9Direct = ""
              }
            , Cmd.none
            )

        ToggleExtraNdc9 ndc ->
            ( { model | selectedExtraNdc9s = toggleSetMember ndc model.selectedExtraNdc9s }
            , Cmd.none
            )

        FetchBooksOfExtraNdc9 ->
            case model.selectedPrefecture of
                Nothing ->
                    ( model, Cmd.none )

                Just pref ->
                    let
                        existingNdc9s =
                            Set.fromList <| Dict.keys model.bookCache

                        extraNdc9sToFetch =
                            Set.diff model.selectedExtraNdc9s existingNdc9s
                                |> Set.map (\x -> String.left 3 x)
                                |> Set.toList

                        newBookCache =
                            Dict.union model.bookCache <|
                                Dict.fromList <|
                                    List.map (\ndc -> ( ndc, urInit ndc )) extraNdc9sToFetch

                        cmdsInitSearch =
                            initSearchBooks pref.key extraNdc9sToFetch
                    in
                    ( { model
                        | extraNdc9ModalOpened = False
                        , predNdc9Query = ""
                        , predNdc9Store = NotLoaded
                        , selectedExtraNdc9s = Set.empty
                        , bookCache = newBookCache
                      }
                    , Cmd.batch (modalState False :: cmdsInitSearch)
                    )

        -- Filters
        BookFilterMsg subMsg ->
            ( { model
                | filters = BookFilter.update subMsg model.filters
                , selectedPagination = 1
              }
            , Cmd.none
            )

        -- Export
        RequestTsv ->
            ( model, exportTsv model )

        RequestCsv ->
            ( model, exportCsv model )

        RequestPrint ->
            ( model, requestPrint True )



-- UPDATE HELPERS


initSearchBooks : String -> List String -> List (Cmd Msg)
initSearchBooks key ndc9s =
    List.map (\n -> requestUnitradByNdc ( key, n )) ndc9s


addUrToBookCache : UnitradResult -> Model -> ( Model, Cmd Msg )
addUrToBookCache ur model =
    let
        newBookCache =
            Dict.insert ur.query.ndc ur model.bookCache

        defaultNdcTab =
            case model.selectedNdcTab of
                Just _ ->
                    model.selectedNdcTab

                Nothing ->
                    Just ur.query.ndc
    in
    ( { model
        | bookCache = newBookCache
        , selectedNdcTab = defaultNdcTab
      }
    , Cmd.none
    )


incrementBookFetchError : Model -> ( Model, Cmd Msg )
incrementBookFetchError model =
    ( { model | bookFetchErrCount = model.bookFetchErrCount + 1 }, Cmd.none )


getMapping : Model -> Mapping
getMapping model =
    case model.mappingStatus of
        MappingReceived um ->
            um.libraries

        _ ->
            Dict.empty


labelNdc9 : Ndc9LabelStore -> String -> String
labelNdc9 ndc9LabelStore ndc9 =
    case ndc9LabelStore of
        Loaded ndc9LabelDict ->
            Dict.get ndc9 ndc9LabelDict
                |> Maybe.map (\x -> ndc9 ++ " " ++ x)
                |> Maybe.withDefault ndc9

        _ ->
            ndc9


sortBooks : List Book -> List Book
sortBooks books =
    List.reverse <| List.sortBy .pubdate books


listSelectedBooks : BookCache -> Set String -> List Book
listSelectedBooks bookCache selectedBookIds =
    Dict.values bookCache
        |> List.map .books
        |> List.concat
        |> List.filter (\b -> Set.member b.id selectedBookIds)
        |> removeDuplicateBooks


removeDuplicateBooks : List Book -> List Book
removeDuplicateBooks books =
    let
        step book ( seen, acc ) =
            if Set.member book.id seen then
                ( seen, acc )

            else
                ( Set.insert book.id seen, book :: acc )
    in
    List.foldl step ( Set.empty, [] ) books
        |> Tuple.second
        |> List.reverse


ndcSelectionCounts : Model -> Dict String Int
ndcSelectionCounts { bookCache, selectedBookIds } =
    Dict.map
        (\_ ur ->
            ur.books
                |> List.filter (\book -> Set.member book.id selectedBookIds)
                |> List.length
        )
        bookCache


totalSelectionCount : Model -> Int
totalSelectionCount model =
    Set.size model.selectedBookIds


selectedBookIdsOfNdc : Model -> Set String
selectedBookIdsOfNdc { bookCache, selectedBookIds, selectedNdcTab } =
    case selectedNdcTab of
        Nothing ->
            Set.empty

        Just ndc9 ->
            bookCache
                |> Dict.get ndc9
                |> Maybe.map .books
                |> Maybe.withDefault []
                |> List.map .id
                |> Set.fromList
                |> Set.intersect selectedBookIds


paginateBookList : Int -> Int -> List Book -> List Book
paginateBookList pageSize currentPage bookList =
    bookList
        |> List.drop (pageSize * (currentPage - 1))
        |> List.take pageSize


openBookModal : Book -> Html.Attribute Msg
openBookModal book =
    onClick <| ToggleBookModal (Opened book)



-- EXPORT HELPERS


fileExportHeader : List String
fileExportHeader =
    [ "タイトル"
    , "著者"
    , "出版者"
    , "出版年"
    , "巻号"
    , "NDC"
    , "ISBN"
    ]


exportBookToStrList : Book -> List String
exportBookToStrList book =
    [ book.title
    , book.author
    , book.publisher
    , book.pubdate
    , book.volume
    , book.ndc
    , book.isbn
    ]


exportTsv : Model -> Cmd Msg
exportTsv model =
    let
        tsvString =
            listSelectedBooks model.bookCache model.selectedBookIds
                |> List.map (\book -> String.join "\t" <| exportBookToStrList book)
                |> String.join "\n"

        tsvHeader =
            String.join "\t" fileExportHeader
    in
    Download.string "bookreach-output.tsv" "text/tab-separated-values" <|
        ("\u{FEFF}" ++ tsvHeader ++ "\n" ++ tsvString)


exportCsv : Model -> Cmd Msg
exportCsv model =
    let
        csvString =
            listSelectedBooks model.bookCache model.selectedBookIds
                |> List.map exportBookToStrList
                |> CsvEn.encode
                    { encoder = CsvEn.withFieldNames (List.map2 Tuple.pair fileExportHeader)
                    , fieldSeparator = ','
                    }
    in
    Download.string "bookreach-output.csv" "text/csv" ("\u{FEFF}" ++ csvString)



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        case model.stage of
            PrefectureSelectingStage ->
                [ viewHero "BookReach Explorer" "図書館の地域を選択してください"
                , viewPrefectureSelector model
                ]

            NdcSelectingStage ->
                [ viewHero "探索NDC選定" "授業について教えてください"
                , viewPrefectureInfo model
                , NdcSelect.view model.ndcSelect |> Html.map NdcSelectMsg
                , viewFetchBooksButton model
                ]

            ExploringStage ->
                [ viewHero "教材探索" "教材候補を選んでください"
                , NdcSelect.viewCompact model.ndcSelect |> Html.map NdcSelectMsg

                -- Modals
                , viewBookModal model
                , viewExtraNdc9Modal model

                -- Main content
                , if model.bookListOpened then
                    viewBooklist model

                  else
                    viewBookBrowser model
                ]


viewHero : String -> String -> Html Msg
viewHero title_ desc =
    section [ class "hero is-small no-print" ]
        [ div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ h1 [ class "title" ] [ text title_ ]
                , p [ class "subtitle" ] [ text desc ]
                ]
            ]
        ]



-- PREFECTURE SELECTOR


viewPrefectureSelector : Model -> Html Msg
viewPrefectureSelector model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ case model.prefectures of
                Loading ->
                    div [ class "has-text-centered" ]
                        [ button [ class "button is-large is-loading is-text" ] [] ]

                LoadErr ->
                    div [ class "notification is-danger" ]
                        [ text "都道府県データの読み込みに失敗しました。ページを再読み込みしてください。" ]

                Loaded prefectures ->
                    viewPrefectureGrid prefectures

                NotLoaded ->
                    div [] []
            ]
        ]


viewPrefectureGrid : List Prefecture -> Html Msg
viewPrefectureGrid prefectures =
    let
        indexed =
            List.indexedMap Tuple.pair prefectures

        regionGroup name start end =
            let
                regionPrefs =
                    List.filter (\( i, _ ) -> i >= start && i <= end) indexed
                        |> List.map Tuple.second
            in
            div [ class "mb-5" ]
                [ h3 [ class "subtitle is-5 mb-2" ] [ text name ]
                , div [ class "buttons" ] (List.map viewPrefectureButton regionPrefs)
                ]
    in
    div []
        [ regionGroup "北海道・東北" 0 6
        , regionGroup "関東" 7 13
        , regionGroup "中部" 14 22
        , regionGroup "近畿" 23 29
        , regionGroup "中国・四国" 30 38
        , regionGroup "九州・沖縄" 39 46
        ]


viewPrefectureButton : Prefecture -> Html Msg
viewPrefectureButton pref =
    button
        [ class "button"
        , onClick (SelectPrefecture pref)
        ]
        [ text pref.label ]


viewPrefectureInfo : Model -> Html Msg
viewPrefectureInfo model =
    case model.selectedPrefecture of
        Just pref ->
            section [ class "section pb-0" ]
                [ div [ class "container" ]
                    [ nav [ class "level" ]
                        [ div [ class "level-left" ]
                            [ div [ class "level-item" ]
                                [ span [ class "icon-text" ]
                                    [ span [ class "icon" ] [ i [ class "fas fa-map-marker-alt" ] [] ]
                                    , span [ class "is-size-5" ]
                                        [ text <| pref.label ++ "（" ++ pref.primaryLibName ++ "）" ]
                                    ]
                                ]
                            ]
                        , div [ class "level-right" ]
                            [ div [ class "level-item" ]
                                [ button
                                    [ class "button is-small is-outlined"
                                    , onClick ChangePrefecture
                                    ]
                                    [ text "地域を変更" ]
                                ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []



-- NDC SELECTING STAGE


viewFetchBooksButton : Model -> Html Msg
viewFetchBooksButton model =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ div [ class "container has-text-centered" ]
                [ button
                    [ class "button is-medium is-success no-print"
                    , onClick FetchBooks
                    , disabled <| not (NdcSelect.canProceed model.ndcSelect)
                    ]
                    [ text "蔵書から教材候補を取得" ]
                , p [ class "help" ]
                    [ text "候補図書を取得後、蔵書探索するNDCを追加・削除できます" ]
                ]
            ]
        ]



-- EXPLORING STAGE: BOOK BROWSER


viewBookBrowser : Model -> Html Msg
viewBookBrowser model =
    let
        bookCnt =
            totalSelectionCount model
    in
    section [ class "section animate__animated animate__fadeIn" ]
        [ div [ class "container" ]
            [ nav [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ p [ class "title" ] [ text "教材候補ブラウザ" ] ]
                    , div [ class "level-item has-text-centered" ]
                        [ div []
                            [ p [ class "heading" ] [ text "表示形式" ]
                            , p [] [ viewBookDisplayModeButtons model ]
                            ]
                        ]
                    , if model.bookDisplayMode == ThumbnailMode then
                        div [ class "level-item has-text-centered" ]
                            [ div []
                                [ p [ class "heading" ] [ text "書影サイズ" ]
                                , p [] [ viewCoverSizeToggle model.coverSize ]
                                ]
                            ]

                      else
                        text ""
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ p
                            [ class "button is-middle is-warning"
                            , onClick ToggleBookList
                            ]
                            [ span [ class "icon" ] [ i [ class "fas fa-lg fa-book" ] [] ]
                            , span [] [ text "図書リスト" ]
                            , if bookCnt > 0 then
                                span [ class "ml-1" ] [ text <| "(" ++ String.fromInt bookCnt ++ "冊)" ]

                              else
                                text ""
                            ]
                        ]
                    ]
                ]
            , BookFilter.view model.filters (getMapping model) |> Html.map BookFilterMsg
            , viewBookBrowserTabbar model
            , viewBookBrowserCore model
            ]
        ]


viewBookDisplayModeButtons : Model -> Html Msg
viewBookDisplayModeButtons model =
    div [ class "level-item buttons are-small has-addons pb-4" ] <|
        List.map
            (viewBookDisplayModeButton model.bookDisplayMode)
            [ ( TableMode, "リスト" ), ( ThumbnailMode, "書影" ) ]


viewBookDisplayModeButton : BookDisplayMode -> ( BookDisplayMode, String ) -> Html Msg
viewBookDisplayModeButton currentMode ( mode, label ) =
    button
        [ class <|
            "button"
                ++ (if currentMode == mode then
                        " is-info is-active"

                    else
                        ""
                   )
        , onClick (ChangeBookDisplayMode mode)
        ]
        [ text label ]


viewCoverSizeToggle : CoverSize -> Html Msg
viewCoverSizeToggle currentSize =
    div [ class "level-item buttons are-small has-addons pb-4" ]
        [ button
            [ classList
                [ ( "button", True )
                , ( "is-info is-active", currentSize == SmallCover )
                ]
            , onClick (SetCoverSize SmallCover)
            ]
            [ span [ class "icon is-small" ] [ i [ class "fas fa-th" ] [] ]
            , span [] [ text "小" ]
            ]
        , button
            [ classList
                [ ( "button", True )
                , ( "is-info is-active", currentSize == LargeCover )
                ]
            , onClick (SetCoverSize LargeCover)
            ]
            [ span [ class "icon is-small" ] [ i [ class "fas fa-th-large" ] [] ]
            , span [] [ text "大" ]
            ]
        ]



-- TABS


viewBookBrowserTabbar : Model -> Html Msg
viewBookBrowserTabbar model =
    div [ class "tabs is-boxed" ]
        [ ul [] <| List.append (viewNdc9Tabs model) viewNewTab ]


viewNewTab : List (Html Msg)
viewNewTab =
    [ li [ onClick ToggleExtraNdc9Modal ]
        [ a [ target "_self", class "icon is-large" ] [ i [ class "fas fa-lg fa-plus" ] [] ] ]
    ]


viewNdc9Tabs : Model -> List (Html Msg)
viewNdc9Tabs model =
    let
        selectionCounts =
            ndcSelectionCounts model
    in
    List.map (viewNdc9Tab model selectionCounts) (Dict.values model.bookCache)


viewNdc9Tab : Model -> Dict String Int -> UnitradResult -> Html Msg
viewNdc9Tab model selectionCounts ur =
    let
        selectedCount =
            Dict.get ur.query.ndc selectionCounts
                |> Maybe.withDefault 0

        hasSelections =
            selectedCount > 0

        labelClasses =
            [ ( "has-text-weight-bold", hasSelections )
            , ( "has-text-info", hasSelections )
            ]

        textNdc9Label =
            [ strong [ classList labelClasses ] [ text <| labelNdc9 model.ndc9LabelStore ur.query.ndc ]
            , if hasSelections then
                span [ class "tag is-rounded is-info is-light ml-2 mr-2" ] [ text <| String.fromInt selectedCount ]

              else
                text ""
            , span [] [ text "(" ]
            ]

        textCnt =
            if ur.count < 0 then
                [ span [] [ text "取得中）" ] ]

            else
                [ span [] [ text <| String.fromInt ur.count ]
                , span [] [ text "冊)" ]
                ]

        icon =
            if ur.running then
                [ button [ class "button is-loading is-small is-ghost" ] [] ]

            else
                [ span
                    [ class "icon is-small"
                    , id "close-ndc9"
                    , onClickSP <| CloseNdc9Tab ur.query.ndc
                    ]
                    [ i [ class "fas fa-times" ] [] ]
                ]
    in
    li
        [ classList [ ( "is-active", model.selectedNdcTab == Just ur.query.ndc ) ]
        , onClick (SelectNdcTab ur.query.ndc)
        ]
        [ a [ target "_self", classList [ ( "has-text-danger", ur.count == 0 ) ] ] <|
            if BookFilter.isFiltered model.filters then
                textNdc9Label
                    ++ [ span
                            [ class "has-text-danger" ]
                            [ text <|
                                (String.fromInt << List.length)
                                    (BookFilter.apply model.filters ur.books)
                            ]
                       , span [] [ text "/" ]
                       ]
                    ++ textCnt
                    ++ icon

            else
                textNdc9Label ++ textCnt ++ icon
        ]



-- BOOK BROWSER CORE


viewBookBrowserCore : Model -> Html Msg
viewBookBrowserCore model =
    let
        filteredBookList =
            case model.selectedNdcTab of
                Nothing ->
                    []

                Just ndc9 ->
                    model.bookCache
                        |> Dict.get ndc9
                        |> Maybe.map .books
                        |> Maybe.withDefault []
                        |> BookFilter.apply model.filters
                        |> sortBooks
    in
    case model.bookDisplayMode of
        ThumbnailMode ->
            div [ class "container" ] <|
                viewBookBrowserThumbnail model filteredBookList

        TableMode ->
            div [ class "container" ] <|
                viewBookBrowserTable model filteredBookList


viewBookBrowserThumbnail : Model -> List Book -> List (Html Msg)
viewBookBrowserThumbnail model bookList =
    let
        pageSize =
            case model.coverSize of
                SmallCover ->
                    36

                LargeCover ->
                    12

        pagedBooks =
            paginateBookList pageSize model.selectedPagination bookList

        selBookIds =
            selectedBookIdsOfNdc model
    in
    [ viewPagination pageSize (List.length bookList) model.selectedPagination
    , Keyed.node "div"
        [ class "columns is-1 is-multiline is-mobile"
        , style "min-height" "40vh"
        ]
        (List.map (viewKeyedBook model.coverSize model.filters selBookIds) pagedBooks)
    ]


viewBookBrowserTable : Model -> List Book -> List (Html Msg)
viewBookBrowserTable model bookList =
    let
        pagedBooks =
            paginateBookList 8 model.selectedPagination bookList

        selBookIds =
            selectedBookIdsOfNdc model
    in
    [ viewPagination 8 (List.length bookList) model.selectedPagination
    , table [ class "table is-fullwidth is-hoverable", id "tableMode" ]
        [ thead []
            [ tr []
                [ th [ style "width" "2.5%" ] []
                , th [ style "width" "5%" ] []
                , th [] [ text "タイトル" ]
                , th [ style "width" "20%" ] [ text "著者" ]
                , th [ style "width" "10%" ] [ text "出版年" ]
                , th [ style "width" "15%" ] [ text "出版者" ]
                ]
            ]
        , tbody []
            (List.map (viewBookBrowserTableRow SmallCover model.filters selBookIds) pagedBooks)
        ]
    ]



-- PAGINATION


viewPagination : Int -> Int -> Int -> Html Msg
viewPagination booksPerPage totalCnt selectedPagination =
    let
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
                , onClick (SelectPagination i)
                ]
                [ text <| String.fromInt i ]
    in
    nav [ class "pagination is-centered is-small" ]
        [ button
            [ class "button pagination-previous"
            , target "_self"
            , onClick (SelectPagination <| Basics.max 1 (selectedPagination - 1))
            , disabled (selectedPagination == 1)
            ]
            [ text "←" ]
        , button
            [ class "button pagination-next"
            , target "_self"
            , onClick (SelectPagination <| Basics.min numPages (selectedPagination + 1))
            , disabled (selectedPagination == numPages || numPages == 0)
            ]
            [ text "→" ]
        , ul [ class "pagination-list" ] <|
            if numPages < 1 then
                [ pLink 1 ]

            else if numPages <= 20 then
                List.map pLink (List.range 1 numPages)

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



-- BOOK CARD (THUMBNAIL)


viewKeyedBook : CoverSize -> BookFilter.State -> Set String -> Book -> ( String, Html Msg )
viewKeyedBook coverSize filters selectedBookIds book =
    ( book.id, lazy4 viewBook coverSize filters selectedBookIds book )


viewBook : CoverSize -> BookFilter.State -> Set String -> Book -> Html Msg
viewBook coverSize filters selectedBookIds book =
    let
        columnClass =
            case coverSize of
                SmallCover ->
                    "column is-1 has-text-left"

                LargeCover ->
                    "column is-2 has-text-left"
    in
    div [ class columnClass ]
        [ div
            [ id "book"
            , class "container"
            , style "padding" "0.5em"
            , style "border-radius" "10px"
            , classList [ ( "has-background-info", Set.member book.id selectedBookIds ) ]
            ]
            [ label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , onClick (SelectBook book)
                    , checked (Set.member book.id selectedBookIds)
                    ]
                    []
                , strong [] (highlightQueries filters <| shortenTitle book.title)
                ]
            , figure
                [ class "image is-clickable"
                , openBookModal book
                ]
                (viewBookCoverImg coverSize book)
            ]
        ]


viewBookCoverImg : CoverSize -> Book -> List (Html Msg)
viewBookCoverImg coverSize book =
    if not (hasIsbn book) then
        let
            fontSize =
                case coverSize of
                    SmallCover ->
                        "0.6em"

                    LargeCover ->
                        "1.5em"
        in
        [ strong
            [ style "position" "absolute"
            , style "top" "10%"
            , style "left" "10%"
            , style "width" "80%"
            , style "font-size" fontSize
            , style "color" "white"
            , style "text-align" "center"
            ]
            [ text (shortenTitle book.title) ]
        , img [ src "assets/blank_cover.png" ] []
        ]

    else
        [ img [ src (extractCoverUrl book) ] [] ]



-- BOOK TABLE ROW


viewBookBrowserTableRow : CoverSize -> BookFilter.State -> Set String -> Book -> Html Msg
viewBookBrowserTableRow coverSize filters selectedBookIds book =
    let
        attrOpenModal =
            [ openBookModal book
            , class "is-clickable"
            ]

        ( thumbWidth, thumbHeight ) =
            case coverSize of
                SmallCover ->
                    ( "30px", "40px" )

                LargeCover ->
                    ( "120px", "160px" )

        bookThumbnail =
            [ img
                [ style "max-width" thumbWidth
                , style "max-height" thumbHeight
                , src <|
                    if hasIsbn book then
                        extractCoverUrl book

                    else
                        "assets/blank_cover.png"
                ]
                []
            ]

        authorList =
            String.split "\t" book.author
    in
    tr []
        [ td [ class "has-text-centered" ]
            [ label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , onClick (SelectBook book)
                    , checked (Set.member book.id selectedBookIds)
                    ]
                    []
                ]
            ]
        , td attrOpenModal bookThumbnail
        , td attrOpenModal
            [ strong [] [ text book.title ]
            , br [] []
            , text book.volume
            ]
        , td attrOpenModal
            [ text
                (if List.length authorList > 1 then
                    String.join "" (List.take 1 authorList) ++ " 他"

                 else
                    Maybe.withDefault "" (List.head authorList)
                )
            ]
        , td attrOpenModal [ text book.pubdate ]
        , td attrOpenModal [ text book.publisher ]
        ]



-- BOOK MODAL


viewBookModal : Model -> Html Msg
viewBookModal model =
    let
        mapping =
            getMapping model

        highlighter =
            highlightQueries model.filters

        bookIsSelected =
            Set.member book.id model.selectedBookIds

        book =
            case model.bookModal of
                Opened b ->
                    b

                Closed ->
                    nullBook

        bibInfoCol =
            [ style "width" "6em", class "has-text-right" ]
    in
    div
        [ class "modal"
        , classList [ ( "is-active", model.bookModal /= Closed ) ]
        ]
        [ div
            [ class "modal-background"
            , onClick (ToggleBookModal Closed)
            ]
            []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ strong [] <| highlighter (shortenTitle book.title) ]
                , button
                    [ attribute "aria-label" "close"
                    , class "delete"
                    , onClick (ToggleBookModal Closed)
                    ]
                    []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "columns is-mobile" ]
                    [ div [ class "column is-4" ]
                        [ figure [ class "image" ] (viewBookCoverImg LargeCover book)
                        , p [ class "buttons" ] <|
                            [ a
                                [ class "button is-small is-warning"
                                , disabled (not (hasIsbn book))
                                , href ("https://www.amazon.co.jp/dp/" ++ book.id)
                                , target "_blank"
                                ]
                                [ span [ class "icon" ]
                                    [ i [ class "fab fa-amazon" ] [] ]
                                , span [] [ text "Amazon" ]
                                ]
                            , a
                                [ class "button is-small is-info"
                                , disabled (not (hasIsbn book))
                                , href ("https://books.google.co.jp/books?vid=ISBN" ++ book.id)
                                , target "_blank"
                                ]
                                [ span [ class "icon" ]
                                    [ i [ class "fab fa-google" ] [] ]
                                , span [] [ text "Google" ]
                                ]
                            ]
                                ++ viewLibraryButtons mapping book
                        ]
                    , div [ class "column" ]
                        [ strong [ class "is-6" ] [ text "図書詳細" ]
                        , table [ class "table is-size-7 is-narrow is-fullwidth" ]
                            [ tr [] [ th bibInfoCol [ text "タイトル" ], td [] <| highlighter book.title ]
                            , tr [] [ th bibInfoCol [ text "著者" ], td [] <| highlighter book.author ]
                            , tr [] [ th bibInfoCol [ text "巻号" ], td [] <| highlighter book.volume ]
                            , tr [] [ th bibInfoCol [ text "出版年" ], td [] <| highlighter book.pubdate ]
                            , tr [] [ th bibInfoCol [ text "出版者" ], td [] <| highlighter book.publisher ]
                            , tr [] [ th bibInfoCol [ text "ISBN" ], td [] [ text book.isbn ] ]
                            , tr [] [ th bibInfoCol [ text "NDC" ], td [] [ text book.ndc ] ]
                            ]
                        ]
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ div [ class "buttons" ]
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
        ]


viewLibraryButtons : Mapping -> Book -> List (Html Msg)
viewLibraryButtons mapping book =
    List.map viewLibraryButton (extractLibNameLinkPairs mapping book)


viewLibraryButton : ( String, String ) -> Html Msg
viewLibraryButton ( libName, libLink ) =
    a
        [ class "button is-small", href libLink, target "_blank" ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-university" ] [] ]
        , span [] [ text libName ]
        ]



-- EXTRA NDC MODAL


viewExtraNdc9Modal : Model -> Html Msg
viewExtraNdc9Modal model =
    let
        existingNdc9s =
            Set.fromList <| Dict.keys model.bookCache

        disableFetchButton =
            disabled <| Set.isEmpty model.selectedExtraNdc9s

        disableAddButton =
            disabled <|
                Set.member model.extraNdc9Direct existingNdc9s
                    || not (checkNdc9 model.extraNdc9Direct)
    in
    div [ class "modal", classList [ ( "is-active", model.extraNdc9ModalOpened ) ] ]
        [ div [ class "modal-background", onClick ToggleExtraNdc9Modal ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ] [ strong [] [ text "NDCを追加" ] ]
                , button [ attribute "aria-label" "close", class "delete", onClick ToggleExtraNdc9Modal ] []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "content" ]
                    [ div [ class "field" ]
                        [ label [ class "label" ] [ text "キーワードをNDCに変換" ]
                        , div [ class "field has-addons" ]
                            [ p [ class "control is-expanded" ]
                                [ input
                                    [ class "input"
                                    , type_ "text"
                                    , placeholder "例）地球温暖化 石油資源"
                                    , value model.predNdc9Query
                                    , onInput EditPredNdc9Query
                                    ]
                                    []
                                , p [ class "help" ]
                                    [ a [ href "https://lab.ndl.go.jp/ndc/" ] [ text "NDC Predictor" ]
                                    , text "を用いて最大3件の関連NDCに変換されます。"
                                    ]
                                ]
                            , p [ class "control" ]
                                [ button
                                    [ class "button"
                                    , classList
                                        [ ( "is-primary", model.predNdc9Store /= LoadErr )
                                        , ( "is-danger", model.predNdc9Store == LoadErr )
                                        , ( "is-loading", model.predNdc9Store == Loading )
                                        ]
                                    , onClick PredictNdc9s
                                    , disabled <| model.predNdc9Query == ""
                                    ]
                                    [ text "変換" ]
                                ]
                            ]
                        ]
                    , viewPredNdc9Table model.predNdc9Store existingNdc9s model.selectedExtraNdc9s
                    , hr [] []
                    , div [ class "field" ]
                        [ label [ class "label" ] [ text "NDCを直接入力" ]
                        , div [ class "field has-addons" ]
                            [ p [ class "control is-expanded" ]
                                [ input
                                    [ class "input"
                                    , type_ "text"
                                    , placeholder "NDC記号または件名"
                                    , list "ndc9"
                                    , value model.extraNdc9Direct
                                    , onInput EditExtraNdc9Direct
                                    ]
                                    []
                                , datalist [ id "ndc9" ] <|
                                    viewNdc9Options model.ndc9LabelStore existingNdc9s
                                ]
                            , p [ class "control" ]
                                [ button
                                    [ class "button is-primary"
                                    , onClick AddExtraNdc9Direct
                                    , disableAddButton
                                    ]
                                    [ text "追加" ]
                                ]
                            ]
                        ]
                    , hr [] []
                    , viewSelectedExtraNdc9Tags model.ndc9LabelStore model.selectedExtraNdc9s
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button
                    [ class "button is-success"
                    , onClick FetchBooksOfExtraNdc9
                    , disableFetchButton
                    ]
                    [ text "選択したNDCの図書を追加検索" ]
                , button
                    [ class "button"
                    , onClick ToggleExtraNdc9Modal
                    ]
                    [ text "キャンセル" ]
                ]
            ]
        ]


viewPredNdc9Table : LocalStore (List PredNdc9) -> Set String -> Set String -> Html Msg
viewPredNdc9Table predNdc9Store fetchedNdc9s selectedExtraNdc9s =
    let
        isAlreadyFetched ndc9 =
            Set.member ndc9 fetchedNdc9s

        roundedScoreString : Float -> String
        roundedScoreString score =
            if score * 1000 < 1 then
                "0.001未満"

            else if score * 1000 > 999 then
                "0.999以上"

            else
                score
                    |> String.fromFloat
                    |> String.left 5

        viewPredNdc9 : PredNdc9 -> Html Msg
        viewPredNdc9 pred =
            let
                ndcLabel =
                    pred.ndc ++ " (" ++ pred.label ++ ")"

                isDisabled =
                    isAlreadyFetched pred.ndc
            in
            tr
                [ class <|
                    if isDisabled then
                        "has-background-light has-text-grey"

                    else
                        ""
                ]
                [ td [] [ text pred.ndc ]
                , td [] [ text pred.label ]
                , td [] [ text (roundedScoreString pred.score) ]
                , td []
                    [ input
                        [ class "checkbox"
                        , type_ "checkbox"
                        , checked (Set.member ndcLabel selectedExtraNdc9s)
                        , onClick (ToggleExtraNdc9 ndcLabel)
                        , disabled isDisabled
                        ]
                        []
                    ]
                ]
    in
    table [ class "table is-fullwidth is-narrow is-hoverable" ] <|
        case predNdc9Store of
            Loaded predNdc9s ->
                [ thead []
                    [ tr []
                        [ th [] [ text "NDC" ]
                        , th [] [ text "ラベル" ]
                        , th [] [ text "関連度スコア" ]
                        , th [] [ text "追加" ]
                        ]
                    ]
                , tbody [] (List.map viewPredNdc9 predNdc9s)
                , tfoot []
                    [ tr []
                        [ th [ class "has-text-weight-light is-size-7", colspan 4 ]
                            [ text "すでに教材候補を取得済みのNDCは灰色で表示されます。新しいフレーズを変換すれば、他のNDCも追加できます。" ]
                        ]
                    ]
                ]

            _ ->
                []


viewSelectedExtraNdc9Tags : Ndc9LabelStore -> Set String -> Html Msg
viewSelectedExtraNdc9Tags store selectedExtraNdc9s =
    let
        viewNdc9Tag : String -> Html Msg
        viewNdc9Tag symbol =
            div [ class "control" ]
                [ div [ class "tags has-addons" ]
                    [ span [ class "tag is-info" ] [ text <| labelNdc9 store symbol ]
                    , a [ class "tag is-delete", target "_self", onClick (ToggleExtraNdc9 symbol) ] []
                    ]
                ]
    in
    div [ class "field" ]
        [ label [ class "label" ] [ text "追加するNDC" ]
        , div [ class "field is-grouped is-grouped-multiline" ] <|
            if Set.isEmpty selectedExtraNdc9s then
                [ div [ class "notification pt-1 pb-2" ] [ text "NDCを追加するとここに表示されます。" ] ]

            else
                List.map viewNdc9Tag (Set.toList selectedExtraNdc9s)
        ]


viewNdc9Options : Ndc9LabelStore -> Set String -> List (Html Msg)
viewNdc9Options ndc9Store fetchedNdc9s =
    let
        viewNdc9Option : ( String, String ) -> Html Msg
        viewNdc9Option ( symbol, lbl ) =
            option [ value symbol, disabled (Set.member symbol fetchedNdc9s) ]
                [ text lbl ]
    in
    case ndc9Store of
        Loaded ndc9s ->
            List.map viewNdc9Option (Dict.toList ndc9s)

        _ ->
            []



-- BOOK LIST


viewBooklist : Model -> Html Msg
viewBooklist model =
    let
        mapping =
            getMapping model

        selectedBookCnt =
            String.fromInt <| Set.size model.selectedBookIds
    in
    section [ class "section" ]
        [ div [ class "container animate__animated animate__fadeIn" ]
            [ nav [ class "level" ]
                [ div [ class "level-left" ]
                    [ h2 [ class "title" ]
                        [ text "図書リスト"
                        , span [ class "tag is-rounded" ]
                            [ text <| selectedBookCnt ++ "冊" ]
                        ]
                    ]
                , viewBooklistExportButtons model
                , div [ class "level-right" ]
                    [ p
                        [ class "button is-middle is-primary no-print"
                        , onClick ToggleBookList
                        ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-columns" ] [] ]
                        , span [] [ text "教材候補ブラウザに戻る" ]
                        ]
                    ]
                ]
            , viewTableOutput mapping model
            ]
        ]


viewBooklistExportButtons : Model -> Html Msg
viewBooklistExportButtons model =
    let
        noBooks =
            Set.isEmpty model.selectedBookIds
    in
    div [ class "level-item no-print" ]
        [ div [ class "buttons" ]
            [ button
                [ class "button"
                , onClick RequestPrint
                , disabled noBooks
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-print" ] [] ]
                , span [] [ text "印刷" ]
                ]
            , button
                [ class "button"
                , onClick RequestCsv
                , disabled noBooks
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-file-csv" ] [] ]
                , span [] [ text "CSV出力" ]
                ]
            , button
                [ class "button"
                , onClick RequestTsv
                , disabled noBooks
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-file-alt" ] [] ]
                , span [] [ text "TSV出力" ]
                ]
            ]
        ]


viewTableOutput : Mapping -> Model -> Html Msg
viewTableOutput mapping model =
    div [ class "columns" ]
        [ div [ class "column is-full" ]
            [ table [ class "table is-fullwidth is-hoverable" ]
                [ thead [ class "has-text-left" ]
                    [ tr []
                        [ th [] []
                        , th [ style "width" "8%" ] []
                        , th [] [ text "タイトル" ]
                        , th [] [ text "著者" ]
                        , th [] [ text "出版年" ]
                        , th [] [ text "出版者" ]
                        , th [] [ text "NDC" ]
                        , th [ style "width" "20%" ] [ text "所蔵" ]
                        ]
                    ]
                , tbody [] (viewTableOutputRows mapping model.bookCache model.selectedBookIds)
                ]
            ]
        ]


viewTableOutputRows : Mapping -> BookCache -> Set String -> List (Html Msg)
viewTableOutputRows mapping bookCache selectedBookIds =
    listSelectedBooks bookCache selectedBookIds
        |> List.map (viewTableOutputRow mapping)


viewTableOutputRow : Mapping -> Book -> Html Msg
viewTableOutputRow mapping book =
    let
        attrOpenModal =
            openBookModal book

        bookThumbnail =
            [ img
                [ style "width" "60px"
                , style "height" "80px"
                , src <|
                    if hasIsbn book then
                        extractCoverUrl book

                    else
                        "assets/blank_cover.png"
                ]
                []
            ]
    in
    tr [ class "is-clickable" ]
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
        , td [ attrOpenModal ] bookThumbnail
        , td [ attrOpenModal ]
            [ strong [] [ text book.title ]
            , br [] []
            , text book.volume
            ]
        , td [ attrOpenModal ] <| List.intersperse (br [] []) (List.map text <| String.split "\t" book.author)
        , td [ attrOpenModal ] [ text book.pubdate ]
        , td [ attrOpenModal ] [ text book.publisher ]
        , td [ attrOpenModal ] [ text book.ndc ]
        , td [ attrOpenModal ]
            [ div [ class "buttons" ] (viewLibraryButtons mapping book) ]
        ]



-- QUERY HIGHLIGHTING


highlightQueries : BookFilter.State -> String -> List (Html Msg)
highlightQueries { queryOn, query } txt =
    let
        queries =
            String.split " " <| String.replace "\u{3000}" " " query

        viewHighlightToken : String -> Html Msg
        viewHighlightToken token =
            if String.startsWith "__" token then
                span
                    [ class "has-background-warning has-text-black" ]
                    [ text <| String.dropLeft 2 token ]

            else
                span [] [ text token ]
    in
    if queryOn then
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
