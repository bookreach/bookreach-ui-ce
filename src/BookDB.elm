module BookDB exposing (..)

import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Parser exposing ((|.), (|=), Parser, andThen, chompWhile, getChompedString, keyword, map, oneOf, problem, run, spaces, succeed, symbol)
import School exposing (School)
import Set exposing (Set)
import Url.Builder as B


apiUrl =
    "https://sample-bookdb.herokuapp.com"



-- TEXTBOOK


type alias TextBook =
    { id : Int
    , title : String
    , grade : String
    , school : String
    }


nullTextBook =
    TextBook 0 "" "" ""


textBooksDecoder : D.Decoder (List TextBook)
textBooksDecoder =
    D.list textBookDecoder


textBookDecoder : D.Decoder TextBook
textBookDecoder =
    D.map4 TextBook
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "grade" D.string)
        (D.field "school" D.string)



-- TANGEN


type alias Tangen =
    { id : Int
    , textBookId : Int
    , number : Int
    , title : String
    , chapters : List Chapter
    }


tangensDecoder : D.Decoder (List Tangen)
tangensDecoder =
    D.list tangenDecoder


tangenDecoder : D.Decoder Tangen
tangenDecoder =
    D.map5 Tangen
        (D.field "id" D.int)
        (D.field "textbookId" D.int)
        (D.field "number" D.int)
        (D.field "title" D.string)
        (D.field "chapters" chaptersDecoder)



-- CHAPTER


type alias Chapter =
    { id : Int
    , tangenId : Int
    , number : Int
    , title : String
    , ndcs : List String -- NDC can be "007"
    }


chaptersDecoder : D.Decoder (List Chapter)
chaptersDecoder =
    let
        chapterDecoder : D.Decoder Chapter
        chapterDecoder =
            D.map5 Chapter
                (D.field "id" D.int)
                (D.field "tangenId" D.int)
                (D.field "number" D.int)
                (D.field "title" D.string)
                (D.field "NDCs" (D.list D.string))
    in
    D.list chapterDecoder



-- NDC LABEL


type alias NdcLabel =
    { ndc : String
    , label : String
    }


ndcLabelDecoder : D.Decoder (List NdcLabel)
ndcLabelDecoder =
    D.list
        (D.map2 NdcLabel
            (D.field "ndc" D.string)
            (D.field "label" D.string)
        )



-- BOOK


type alias Book =
    { id : Int
    , title : String
    , subtitle : String
    , authors : List String
    , publishers : List String
    , year : Int
    , pages : String
    , size : String
    , topics : List String
    , ndc : String
    , ndc_full : String
    , ndc1 : Int
    , notes : List String
    , toc : String
    , target : String
    , isbn10 : String
    , isbn13 : String
    , cover : String
    , caseIds : List Int
    , libcom : String
    , local : Bool
    }


nullBook =
    -- used in viewBookModal
    Book 0 "" "" [] [] 0 "" "" [] "" "" 0 [] "" "" "" "" "" [] "" False


booksDecoder : D.Decoder (List Book)
booksDecoder =
    D.list
        (D.succeed Book
            |> required "id" D.int
            |> required "title" D.string
            |> required "subtitle" D.string
            |> required "authors" (D.list D.string)
            |> required "publishers" (D.list D.string)
            |> required "year" D.int
            |> required "pages" D.string
            |> required "size" D.string
            |> required "topics" (D.list D.string)
            |> required "ndc" D.string
            |> required "ndc_full" D.string
            |> required "ndc1" D.int
            |> required "notes" (D.list D.string)
            |> required "toc" D.string
            |> required "target" D.string
            |> required "isbn10" D.string
            |> required "isbn13" D.string
            |> required "cover" D.string
            |> required "caseIds" (D.list D.int)
            |> required "libcom" D.string
            |> required "local" D.bool
        )


type alias KyCase =
    { id : Int
    , caseident : String
    , url : String
    , school : String
    , subject : String
    , target : String
    , tangen : String
    , date : String
    , purpose : String
    }


kyCaseDecoder : D.Decoder (List KyCase)
kyCaseDecoder =
    D.list
        (D.succeed KyCase
            |> required "id" D.int
            |> required "caseident" D.string
            |> required "url" D.string
            |> required "school" D.string
            |> required "subject" D.string
            |> required "target" D.string
            |> required "tangen" D.string
            |> required "date" D.string
            |> required "purpose" D.string
        )



-- HTTP


getTextBooks : (Result Http.Error (List TextBook) -> msg) -> School -> Cmd msg
getTextBooks toMsg school =
    Http.get
        { url =
            B.crossOrigin apiUrl
                [ "textbooks" ]
                [ B.string "school" <| School.toString school ]
        , expect = Http.expectJson toMsg textBooksDecoder
        }


getTextBook : (Result Http.Error TextBook -> msg) -> Int -> Cmd msg
getTextBook toMsg textBookId =
    Http.get
        { url =
            B.crossOrigin apiUrl
                [ "textbooks", String.fromInt textBookId ]
                []
        , expect = Http.expectJson toMsg textBookDecoder
        }


getTangens : (Result Http.Error (List Tangen) -> msg) -> Int -> Cmd msg
getTangens toMsg textBookId =
    Http.get
        { url =
            B.crossOrigin apiUrl
                [ "tangens" ]
                [ B.int "textbookId" textBookId, B.string "_embed" "chapters" ]
        , expect = Http.expectJson toMsg tangensDecoder
        }


getTextBooksByIds : (Result Http.Error (List TextBook) -> msg) -> List Int -> Cmd msg
getTextBooksByIds toMsg textBookIds =
    Http.get
        { url =
            B.crossOrigin apiUrl [ "textbooks" ] <| List.map (B.int "id") textBookIds
        , expect = Http.expectJson toMsg textBooksDecoder
        }


getChaptersByIds : (Result Http.Error (List Chapter) -> msg) -> List Int -> Cmd msg
getChaptersByIds toMsg chapterIds =
    Http.get
        { url =
            B.crossOrigin apiUrl [ "chapters" ] <| List.map (B.int "id") chapterIds
        , expect = Http.expectJson toMsg chaptersDecoder
        }


getBooks : (Result Http.Error (List Book) -> msg) -> Set String -> Cmd msg
getBooks toMsg ndcSet =
    let
        ndcQueryParams =
            ndcSet
                |> Set.toList
                |> List.map (B.string "ndc")
    in
    Http.get
        { url =
            B.crossOrigin apiUrl [ "books" ] ndcQueryParams
        , expect = Http.expectJson toMsg booksDecoder
        }


getBooksByIds : (Result Http.Error (List Book) -> msg) -> List Int -> Cmd msg
getBooksByIds toMsg bookIds =
    Http.get
        { url =
            B.crossOrigin apiUrl [ "books" ] <| List.map (B.int "id") bookIds
        , expect = Http.expectJson toMsg booksDecoder
        }


getBooksByQuery : (Result Http.Error (List Book) -> msg) -> String -> Cmd msg
getBooksByQuery toMsg query =
    Http.get
        { url =
            B.crossOrigin apiUrl [ "books" ] [ B.string "q" query ]
        , expect = Http.expectJson toMsg booksDecoder
        }


getBooksWithQuery : (Result Http.Error (List Book) -> msg) -> String -> String -> Cmd msg
getBooksWithQuery toMsg ndc query =
    Http.get
        { url =
            B.crossOrigin apiUrl [ "books" ] [ B.string "q" query, B.string "ndc" ndc ]
        , expect = Http.expectJson toMsg booksDecoder
        }


type alias AdvSearchQuery =
    { query : Maybe String, ndc : Maybe String }


advSearchQueryParser : Parser AdvSearchQuery
advSearchQueryParser =
    oneOf
        [ succeed (AdvSearchQuery Nothing)
            |= map Just ndcParser
        , succeed AdvSearchQuery
            |= queryParser
            |= queryInNdcParser
        ]


ndcParser : Parser String
ndcParser =
    let
        checkNdc code =
            if String.length code == 3 then
                succeed code

            else
                problem "NDC should be three digits"
    in
    getChompedString (chompWhile Char.isDigit)
        |> andThen checkNdc


queryParser : Parser (Maybe String)
queryParser =
    oneOf
        [ succeed Just
            |. symbol "“"
            |= getChompedString (Parser.chompUntil "”")
            |. symbol "”"
        , succeed Nothing
        ]


queryInNdcParser : Parser (Maybe String)
queryInNdcParser =
    oneOf
        [ succeed Just
            |. spaces
            |. keyword "in"
            |. spaces
            |= ndcParser
        , succeed Nothing
        ]


getBooksViaAdvSearch :
    (Maybe String -> Maybe String -> Result Http.Error (List Book) -> msg)
    -> String
    -> Cmd msg
getBooksViaAdvSearch toMsg advSearchStr =
    let
        parsed =
            run advSearchQueryParser advSearchStr
    in
    case parsed of
        Ok { query, ndc } ->
            let
                toMsg_ =
                    toMsg query ndc
            in
            case ( query, ndc ) of
                ( Nothing, Just ndc_ ) ->
                    getBooks toMsg_ <| Set.fromList [ ndc_ ]

                ( Just query_, Nothing ) ->
                    getBooksByQuery toMsg_ query_

                ( Just query_, Just ndc_ ) ->
                    getBooksWithQuery toMsg_ ndc_ query_

                ( Nothing, Nothing ) ->
                    Cmd.none

        Err _ ->
            Cmd.none


getKyCase : (Result Http.Error (List KyCase) -> msg) -> List Int -> Cmd msg
getKyCase toMsg caseIds =
    let
        caseQueryParams =
            List.map (B.int "id") caseIds
    in
    Http.get
        { url = B.crossOrigin apiUrl [ "cases" ] caseQueryParams
        , expect = Http.expectJson toMsg kyCaseDecoder
        }


getNdcLabels : (Result Http.Error (List NdcLabel) -> msg) -> Set String -> Cmd msg
getNdcLabels toMsg ndcSet =
    let
        ndcQueryParams =
            ndcSet
                |> Set.toList
                |> List.map (B.string "ndc")
    in
    Http.get
        { url = B.crossOrigin apiUrl [ "ndcs" ] ndcQueryParams
        , expect = Http.expectJson toMsg ndcLabelDecoder
        }


getAllNdcLabels : (Result Http.Error (List NdcLabel) -> msg) -> Cmd msg
getAllNdcLabels toMsg =
    Http.get
        { url = B.crossOrigin apiUrl [ "ndcs" ] []
        , expect = Http.expectJson toMsg ndcLabelDecoder
        }
