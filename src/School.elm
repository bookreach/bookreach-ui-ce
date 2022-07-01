module School exposing (..)


type School
    = Elementary
    | JuniorHigh
    | High


all : List School
all =
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
