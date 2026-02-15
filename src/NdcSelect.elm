module NdcSelect exposing (Model, Msg, canProceed, initModel, toSelectedNdc9Symbols, toSelectedNdc9s, update, view, viewCompact)

import Api exposing (..)
import Delay
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import School exposing (School(..))
import Set exposing (Set)
import Utils exposing (LocalStore(..), toggleSetMember)



-- Model


type alias PredNdc9Store =
    LocalStore (List PredNdc9)


type alias Model =
    { selectedSchool : Maybe School
    , selectedGrades : Set Int
    , selectedSubject : Maybe String
    , pictureBookIncluded : Bool
    , query : String

    -- Free-text tangen mode
    , freeTextTangen : String
    , predNdc9StoreForFreeTextTangen : PredNdc9Store

    -- Query-based prediction
    , predNdc9StoreForQuery : PredNdc9Store
    }


initModel : Model
initModel =
    { selectedSchool = Nothing
    , selectedGrades = Set.empty
    , selectedSubject = Nothing
    , pictureBookIncluded = False
    , query = ""
    , freeTextTangen = ""
    , predNdc9StoreForFreeTextTangen = NotLoaded
    , predNdc9StoreForQuery = NotLoaded
    }



-- Helpers


extractPredNdc9s : PredNdc9Store -> List PredNdc9
extractPredNdc9s store =
    case store of
        Loaded predNdc9s ->
            predNdc9s

        _ ->
            []


toSelectedNdc9s : Model -> List PredNdc9
toSelectedNdc9s model =
    let
        ndc9sFromTangen =
            extractPredNdc9s model.predNdc9StoreForFreeTextTangen

        ndc9sFromQuery =
            extractPredNdc9s model.predNdc9StoreForQuery

        ndc9s =
            List.concat [ ndc9sFromTangen, ndc9sFromQuery ]
    in
    if model.pictureBookIncluded then
        PredNdc9 "E" "絵本など" -1.0 :: ndc9s

    else
        ndc9s


toSelectedNdc9Symbols : Model -> List String
toSelectedNdc9Symbols model =
    toSelectedNdc9s model
        |> List.map .ndc


canProceed : Model -> Bool
canProceed model =
    not (List.isEmpty (toSelectedNdc9s model))



-- Update


type Msg
    = SelectSchool School
    | SelectSubject String
    | ToggleGrade Int
    | IncludePictureBook Bool
    | EditQuery String
    | WaitPredNdc9sForQuery String
    | GotPredNdc9sForQuery (Result Http.Error (List PredNdc9))
    | EditFreeTextTangen String
    | WaitPredNdc9sForFreeTextTangen String
    | GotPredNdc9sForFreeTextTangen (Result Http.Error (List PredNdc9))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectSchool school ->
            ( { initModel | selectedSchool = Just school }
            , Cmd.none
            )

        SelectSubject subject ->
            ( { initModel
                | selectedSchool = model.selectedSchool
                , selectedSubject = Just subject
              }
            , Cmd.none
            )

        ToggleGrade grade ->
            ( { initModel
                | selectedSchool = model.selectedSchool
                , selectedSubject = model.selectedSubject
                , selectedGrades = toggleSetMember grade model.selectedGrades
              }
            , Cmd.none
            )

        IncludePictureBook include ->
            ( { model | pictureBookIncluded = include }, Cmd.none )

        EditQuery query ->
            if query /= "" then
                ( { model | query = query }, Delay.after 500 (WaitPredNdc9sForQuery query) )

            else
                ( { model | query = query, predNdc9StoreForQuery = NotLoaded }, Cmd.none )

        WaitPredNdc9sForQuery query ->
            if query == model.query && model.query /= "" then
                ( { model | predNdc9StoreForQuery = Loading }, getPredNdc9s GotPredNdc9sForQuery model.query )

            else
                ( model, Cmd.none )

        GotPredNdc9sForQuery result ->
            case result of
                Ok predNdc9s ->
                    ( { model | predNdc9StoreForQuery = Loaded predNdc9s }, Cmd.none )

                Err _ ->
                    ( { model | predNdc9StoreForQuery = LoadErr }, Cmd.none )

        EditFreeTextTangen text ->
            if text /= "" then
                ( { model | freeTextTangen = text }, Delay.after 500 (WaitPredNdc9sForFreeTextTangen text) )

            else
                ( { model | freeTextTangen = text, predNdc9StoreForFreeTextTangen = NotLoaded }, Cmd.none )

        WaitPredNdc9sForFreeTextTangen text ->
            if text == model.freeTextTangen && model.freeTextTangen /= "" then
                ( { model | predNdc9StoreForFreeTextTangen = Loading }, getPredNdc9s GotPredNdc9sForFreeTextTangen model.freeTextTangen )

            else
                ( model, Cmd.none )

        GotPredNdc9sForFreeTextTangen result ->
            case result of
                Ok predNdc9s ->
                    ( { model | predNdc9StoreForFreeTextTangen = Loaded predNdc9s }, Cmd.none )

                Err _ ->
                    ( { model | predNdc9StoreForFreeTextTangen = LoadErr }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column" ] (viewFields model) ]
            , viewSelectedNdc9s model
            ]
        ]


viewFields : Model -> List (Html Msg)
viewFields model =
    let
        isSogo =
            case model.selectedSubject of
                Just subject ->
                    School.isSogoSubject subject

                Nothing ->
                    False

        baseFilled =
            (model.selectedSchool /= Nothing) && (model.selectedSubject /= Nothing)

        gradeFilled =
            baseFilled && (model.selectedGrades /= Set.empty)

        tangenFilled =
            not (String.isEmpty model.freeTextTangen)

        fieldLabel =
            if isSogo then
                "テーマ・ねらい"

            else
                "単元名"
    in
    List.map Tuple.first <|
        List.filter Tuple.second
            [ ( viewSchoolField model.selectedSchool, True )
            , ( viewSubjectField model.selectedSchool model.selectedSubject, True )
            , ( viewGradeField model.selectedSchool model.selectedGrades, baseFilled )
            , ( viewFreeTextTangenField model, gradeFilled )
            , ( viewSogoQueryField model, tangenFilled )
            ]


viewSelectedNdc9s : Model -> Html Msg
viewSelectedNdc9s model =
    let
        formFilled =
            not (String.isEmpty model.freeTextTangen) || (model.query /= "")
    in
    if formFilled then
        let
            viewNdc9Tag predNdc9 =
                span
                    [ class "tag is-link is-light" ]
                    [ text <| predNdc9.ndc ++ " " ++ predNdc9.label ]

            predNdc9List =
                toSelectedNdc9s model
        in
        div [ class "columns" ]
            [ div [ class "column is-half is-offset-one-quarter has-text-centered", style "border-radius" "20px" ]
                [ div [ class "box" ]
                    [ h2 [ class "subtitle is-4" ] [ text "探索対象のNDC" ]
                    , div [ class "tags is-centered are-medium" ] (List.map viewNdc9Tag predNdc9List)
                    , viewPictureBookEnabler model.pictureBookIncluded
                    ]
                ]
            ]

    else
        div [] []


viewCompact : Model -> Html Msg
viewCompact model =
    let
        isSogo =
            School.isSogoSubject <| Maybe.withDefault "" model.selectedSubject

        tangenLabel =
            if isSogo then
                "テーマ・ねらい"

            else
                "単元（自由入力）"
    in
    section [ class "section pb-0 animate__animated animate__fadeInDown" ]
        [ div [ class "container" ]
            [ nav [ class "level is-mobile" ]
                [ viewCompactItem "校種" <| School.toString <| Maybe.withDefault Elementary model.selectedSchool
                , viewCompactItem "学年" <| String.join " " <| List.map (\i -> String.fromInt i ++ "年") (Set.toList model.selectedGrades)
                , viewCompactItem "科目" <| Maybe.withDefault "" model.selectedSubject
                , viewCompactItem tangenLabel model.freeTextTangen
                ]
            ]
        ]


viewCompactItem : String -> String -> Html Msg
viewCompactItem heading title =
    div [ class "level-item has-text-centered" ]
        [ div []
            [ p [ class "heading" ] [ text heading ]
            , p [ class "title is-5" ] [ text title ]
            ]
        ]



-- Common


viewSchoolField : Maybe School -> Html Msg
viewSchoolField selectedSchool =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-medium" ]
            [ label [ class "label" ] [ text "校種" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control buttons" ] <|
                    List.map (viewSchoolButton selectedSchool) School.listAll
                ]
            ]
        ]


viewSchoolButton : Maybe School -> School -> Html Msg
viewSchoolButton maybeSchool school =
    let
        schoolButtonClasses =
            case maybeSchool of
                Just selectedSchool ->
                    [ ( "is-info", selectedSchool == school )
                    ]

                Nothing ->
                    []
    in
    button
        [ class "button"
        , classList schoolButtonClasses
        , onClick (SelectSchool school)
        ]
        [ text (School.toString school) ]


viewSubjectField : Maybe School -> Maybe String -> Html Msg
viewSubjectField selectedSchool selectedSubject =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-medium" ]
            [ label [ class "label" ] [ text "科目" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control buttons" ] <|
                    List.map (viewFieldButtonHelper SelectSubject selectedSubject) (School.maybeToSubjectList selectedSchool)
                ]
            ]
        ]


viewGradeField : Maybe School -> Set Int -> Html Msg
viewGradeField selectedSchool selectedGrades =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-medium" ]
            [ label [ class "label" ] [ text "学年" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ] <|
                    viewTextbookGrades selectedSchool selectedGrades
                ]
            ]
        ]


viewTextbookGrades : Maybe School -> Set Int -> List (Html Msg)
viewTextbookGrades maybeSchool selectedGrades =
    let
        gradeCheckbox grade =
            label [ class "checkbox is-size-5 pr-2 mt-2" ]
                [ input
                    [ type_ "checkbox"
                    , checked (Set.member grade selectedGrades)
                    , onClick (ToggleGrade grade)
                    ]
                    []
                , text <| " " ++ String.fromInt grade ++ "年"
                ]
    in
    case maybeSchool of
        Just school ->
            List.map gradeCheckbox (School.toGradeList school)

        Nothing ->
            []


viewFieldButtonHelper : (String -> msg) -> Maybe String -> String -> Html msg
viewFieldButtonHelper selectIt maybeSelected toShow =
    let
        buttonClasses =
            case maybeSelected of
                Just selected ->
                    [ ( "is-info", selected == toShow )
                    ]

                Nothing ->
                    []
    in
    button
        [ class "button"
        , classList buttonClasses
        , onClick (selectIt toShow)
        ]
        [ text toShow ]


viewFreeTextTangenField : Model -> Html Msg
viewFreeTextTangenField model =
    let
        isSogo =
            Maybe.withDefault False <| Maybe.map School.isSogoSubject model.selectedSubject

        fieldLabel =
            if isSogo then
                "テーマ・ねらい"

            else
                "単元名"

        fieldPlaceholder =
            if isSogo then
                "キーワードまたは文章から関連NDCを推定します"

            else
                "単元名やテーマを入力すると関連NDCを推定します"
    in
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-medium" ]
            [ label [ class "label" ] [ text fieldLabel ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div
                    [ class <|
                        "control is-medium"
                            ++ (if model.predNdc9StoreForFreeTextTangen == Loading then
                                    " is-loading"

                                else
                                    ""
                               )
                    ]
                    [ input
                        [ class "input is-medium"
                        , type_ "text"
                        , placeholder fieldPlaceholder
                        , value model.freeTextTangen
                        , onInput EditFreeTextTangen
                        ]
                        []
                    ]
                ]
            ]
        ]


viewSogoQueryField : Model -> Html Msg
viewSogoQueryField model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-medium" ]
            [ label [ class "label" ] [ text "追加キーワード" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div
                    [ class <|
                        "control is-medium"
                            ++ (if model.predNdc9StoreForQuery == Loading then
                                    " is-loading"

                                else
                                    ""
                               )
                    ]
                    [ input
                        [ class "input is-medium"
                        , type_ "text"
                        , placeholder "キーワードから探索対象NDCをさらに追加できます"
                        , value model.query
                        , onInput EditQuery
                        ]
                        []
                    ]
                ]
            ]
        ]


viewPictureBookEnabler : Bool -> Html Msg
viewPictureBookEnabler pictureBookIncluded =
    label [ class "checkbox" ]
        [ input
            [ type_ "checkbox"
            , checked pictureBookIncluded
            , onClick (IncludePictureBook (not pictureBookIncluded))
            ]
            []
        , span [ class "ml-2" ] [ text "絵本を探索対象に含める" ]
        ]
