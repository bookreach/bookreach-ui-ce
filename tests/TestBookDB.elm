module TestBookDB exposing (..)

import BookDB exposing (..)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Test advSearchQueryParser"
        [ testAdvSearchQueryParser
            "Parse AdvSearch Query"
            "“ハロー”"
            (AdvSearchQuery (Just "ハロー") Nothing)
        , testAdvSearchQueryParser
            "Parse AdvSearch QueryInNdc"
            "“検索クエリ” in 987"
            (AdvSearchQuery (Just "検索クエリ") (Just "987"))
        , testAdvSearchQueryParser
            "Parse AdvSearch Query 1 char"
            "“猫”"
            (AdvSearchQuery (Just "猫") Nothing)
        ]


testAdvSearchQueryParser : String -> String -> AdvSearchQuery -> Test
testAdvSearchQueryParser desc advSearchStr parsed =
    test desc <|
        \_ ->
            case Parser.run advSearchQueryParser advSearchStr of
                Ok result ->
                    result |> Expect.equal parsed

                Err err ->
                    Expect.fail <|
                        String.join "; " (List.map Debug.toString err)
