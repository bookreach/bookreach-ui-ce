module Api exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E



-- NDC LABEL


type alias Ndc9 =
    { symbol : String -- NDC can be "007"
    , label : String
    }


ndc9ListDecoder : D.Decoder (List Ndc9)
ndc9ListDecoder =
    D.list
        (D.map2 Ndc9
            (D.field "symbol" D.string)
            (D.field "label" D.string)
        )


checkNdc9 : String -> Bool
checkNdc9 symbol =
    case String.toInt symbol of
        Nothing ->
            if symbol == "E" then
                True

            else
                False

        Just ndc9Int ->
            if ndc9Int < 0 || ndc9Int > 999 then
                False

            else
                True


type alias PredNdc9 =
    { ndc : String
    , label : String
    , score : Float
    }


predNdc9ListDecoder : D.Decoder (List PredNdc9)
predNdc9ListDecoder =
    D.list
        (D.map3 PredNdc9
            (D.field "value" D.string)
            (D.field "label" D.string)
            (D.field "score" D.float)
        )



-- BOOK


type alias Book =
    { id : String
    , title : String
    , author : String
    , publisher : String
    , pubdate : String
    , volume : String
    , isbn : String
    , ndc : String
    , holdings : List Int
    , url : Dict String String
    }


nullBook : Book
nullBook =
    Book "" "" "" "" "" "" "" "" [] Dict.empty


bookDecoder : D.Decoder Book
bookDecoder =
    D.succeed Book
        |> required "id" D.string
        |> optional "title" D.string ""
        |> optional "author" D.string ""
        |> optional "publisher" D.string ""
        |> optional "pubdate" pubdateDecoder ""
        |> optional "volume" D.string ""
        |> optional "isbn" D.string ""
        |> optional "ndc" D.string ""
        |> optional "holdings" (D.list D.int) []
        |> optional "url" (D.dict D.string) Dict.empty


pubdateDecoder : D.Decoder String
pubdateDecoder =
    D.oneOf
        [ D.string
        , D.map String.fromInt D.int
        ]


bookListDecoder : D.Decoder (List Book)
bookListDecoder =
    D.list bookDecoder


bookEncoder : Book -> E.Value
bookEncoder book =
    let
        e_holdings =
            book.holdings
                |> List.map String.fromInt
                |> String.join ","
                |> E.string

        e_url =
            book.url
                |> Dict.toList
                |> List.map (\( libId, url ) -> libId ++ "\t" ++ url)
                |> String.join "\n"
                |> E.string
    in
    E.object
        [ ( "id", E.string book.id )
        , ( "title", E.string book.title )
        , ( "author", E.string book.author )
        , ( "publisher", E.string book.publisher )
        , ( "pubdate", E.string book.pubdate )
        , ( "volume", E.string book.volume )
        , ( "isbn", E.string book.isbn )
        , ( "ndc", E.string book.ndc )
        , ( "holdings", e_holdings )
        , ( "url", e_url )
        ]


hasIsbn : Book -> Bool
hasIsbn book =
    let
        id =
            book.id
    in
    (String.length id == 10)
        && String.all Char.isDigit (String.left 9 id)
        && (String.all Char.isDigit (String.right 1 id) || String.endsWith "X" id)


shortenTitle : String -> String
shortenTitle title =
    if String.length title > 20 then
        String.left 20 title ++ "..."

    else
        title


extractLibNameLinkPairs : Mapping -> Book -> List ( String, String )
extractLibNameLinkPairs mapping book =
    Dict.toList book.url
        |> List.map
            (\( libId, url ) ->
                ( Dict.get libId mapping |> Maybe.withDefault "", url )
            )
        |> List.filter (\( libName, _ ) -> not (String.isEmpty libName))


extractCoverUrl : Book -> String
extractCoverUrl book =
    "https://cover.openbd.jp/" ++ book.id ++ ".jpg"



-- Unitrad API Results


type alias UnitradQuery =
    { author : String
    , free : String
    , isbn : String
    , ndc : String
    , publisher : String
    , region : String
    , title : String
    , year_end : String
    , year_start : String
    }


unitradQueryDecoder : D.Decoder UnitradQuery
unitradQueryDecoder =
    D.succeed UnitradQuery
        |> optional "author" D.string ""
        |> optional "free" D.string ""
        |> optional "isbn" D.string ""
        |> optional "ndc" D.string ""
        |> optional "publisher" D.string ""
        |> required "region" D.string
        |> optional "title" D.string ""
        |> optional "year_end" D.string ""
        |> optional "year_start" D.string ""


type alias UnitradResult =
    { uuid : String
    , version : Int
    , query : UnitradQuery
    , count : Int
    , books : List Book
    , running : Bool
    , error : Bool
    }


unitradResultDecoder : D.Decoder UnitradResult
unitradResultDecoder =
    D.succeed UnitradResult
        |> required "uuid" D.string
        |> required "version" D.int
        |> required "query" unitradQueryDecoder
        |> required "count" D.int
        |> required "books" bookListDecoder
        |> required "running" D.bool
        |> hardcoded False


urInit : String -> UnitradResult
urInit ndc =
    { uuid = ""
    , version = 0
    , query = UnitradQuery "" "" "" ndc "" "" "" "" ""
    , count = -1
    , books = []
    , running = True
    , error = False
    }


urError : String -> UnitradResult
urError ndc =
    { uuid = ""
    , version = 0
    , query = UnitradQuery "" "" "" ndc "" "" "" "" ""
    , count = 0
    , books = []
    , running = False
    , error = True
    }


type alias UnitradMapping =
    { libraries : Mapping
    }


type alias Mapping =
    Dict String String


unitradMappingDecoder : D.Decoder UnitradMapping
unitradMappingDecoder =
    D.succeed UnitradMapping
        |> required "libraries" (D.dict D.string)


type MappingStatus
    = MappingNotRequested
    | MappingRequested
    | MappingReceived UnitradMapping
    | MappingError


normaliseUM : String -> UnitradMapping -> UnitradMapping
normaliseUM regionKeyLibId um =
    if Dict.size um.libraries == 1 then
        um

    else
        case Dict.get regionKeyLibId um.libraries of
            Just _ ->
                UnitradMapping (Dict.remove regionKeyLibId um.libraries)

            Nothing ->
                um


mappingToLibraries : Mapping -> List ( Int, String )
mappingToLibraries mapping =
    Dict.toList mapping
        |> List.map
            (\( libId, libName ) ->
                ( String.toInt libId |> Maybe.withDefault 0, libName )
            )
        |> List.filter (\( libId, _ ) -> libId > 0)



-- Prefecture


type alias Prefecture =
    { key : String
    , label : String
    , primaryLibId : String
    , primaryLibName : String
    }


prefectureDecoder : D.Decoder Prefecture
prefectureDecoder =
    D.succeed Prefecture
        |> required "key" D.string
        |> required "label" D.string
        |> required "primary_lib_id" D.string
        |> required "primary_lib_name" D.string



-- HTTP


getPredNdc9s : (Result Http.Error (List PredNdc9) -> msg) -> String -> Cmd msg
getPredNdc9s toMsg keywords =
    Http.post
        { url = "https://lab.ndl.go.jp/ndc/api/predict"
        , body =
            Http.stringBody
                "application/x-www-form-urlencoded"
                ("bib=" ++ keywords)
        , expect = Http.expectJson toMsg predNdc9ListDecoder
        }


loadPrefectures : (Result Http.Error (List Prefecture) -> msg) -> Cmd msg
loadPrefectures toMsg =
    Http.get
        { url = "data/prefectures.json"
        , expect = Http.expectJson toMsg (D.list prefectureDecoder)
        }


loadNdc9Labels : (Result Http.Error (List Ndc9) -> msg) -> Cmd msg
loadNdc9Labels toMsg =
    Http.get
        { url = "data/ndc9-lv3.json"
        , expect = Http.expectJson toMsg ndc9ListDecoder
        }
