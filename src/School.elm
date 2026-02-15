module School exposing (..)

import Set exposing (Set)


type School
    = Elementary
    | JuniorHigh
    | High


listAll : List School
listAll =
    [ Elementary, JuniorHigh, High ]


toString : School -> String
toString school =
    case school of
        Elementary ->
            "小学校"

        JuniorHigh ->
            "中学校"

        High ->
            "高校"


fromString : String -> Maybe School
fromString schoolName =
    case schoolName of
        "小学校" ->
            Just Elementary

        "中学校" ->
            Just JuniorHigh

        "高校" ->
            Just High

        _ ->
            Nothing


toChar : School -> String
toChar school =
    case school of
        Elementary ->
            "e"

        JuniorHigh ->
            "j"

        High ->
            "h"


fromChar : String -> Maybe School
fromChar schoolChar =
    case schoolChar of
        "e" ->
            Just Elementary

        "j" ->
            Just JuniorHigh

        "h" ->
            Just High

        _ ->
            Nothing


toSubjectList : School -> List String
toSubjectList school =
    case school of
        Elementary ->
            [ "国語", "算数", "理科", "社会", "外国語", "生活", "音楽", "図工", "家庭", "保健", "総合" ]

        JuniorHigh ->
            [ "国語", "数学", "理科", "社会（地理的分野）", "社会（歴史的分野）", "社会（公民的分野）", "英語", "音楽", "技術・家庭", "美術", "保健体育", "書写", "地図", "総合" ]

        High ->
            [ "総合" ]


maybeToSubjectList : Maybe School -> List String
maybeToSubjectList maybeSchool =
    case maybeSchool of
        Just school ->
            toSubjectList school

        Nothing ->
            []


isSogoSubject : String -> Bool
isSogoSubject subject =
    if String.contains "総合" subject then
        True

    else
        False


toGradeList : School -> List Int
toGradeList school =
    case school of
        Elementary ->
            [ 1, 2, 3, 4, 5, 6 ]

        _ ->
            [ 1, 2, 3 ]


toGradeSet : School -> Set Int
toGradeSet school =
    Set.fromList (toGradeList school)
