module Main exposing (main)

import Browser
import Count
import Html exposing (Html, div, input, label, table, td, text, tr)
import Html.Attributes as Attr exposing (height, step, type_, value, width)
import Html.Events exposing (onInput)
import Svg
import Svg.Attributes exposing (fill, x, y)


type alias Model =
    { domain : Int
    , codomain : Int
    }


type alias FunctionCounts =
    { allFunctions : Int
    , injective : Int
    , surjective : Int
    , bijective : Int
    }


type alias Proportions =
    { noInjNoSur : Int
    , noInjYesSur : Int
    , yesInjNoSur : Int
    , yesInjYesSur : Int
    }


countFunctions : Model -> FunctionCounts
countFunctions { domain, codomain } =
    { allFunctions = Count.allFunctions domain codomain
    , injective = Count.injective domain codomain
    , surjective = Count.surjective domain codomain
    , bijective = Count.bijective domain codomain
    }


countProportions : FunctionCounts -> Proportions
countProportions { allFunctions, injective, surjective, bijective } =
    { noInjNoSur = allFunctions - injective - surjective + bijective
    , noInjYesSur = surjective - bijective
    , yesInjNoSur = injective - bijective
    , yesInjYesSur = bijective
    }


type Msg
    = ChangeDomain String
    | ChangeCodomain String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { domain = 1
    , codomain = 1
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDomain d ->
            let
                newDomain =
                    String.toInt d |> Maybe.withDefault 0
            in
            { model | domain = newDomain }

        ChangeCodomain c ->
            let
                newCodomain =
                    String.toInt c |> Maybe.withDefault 0
            in
            { model | codomain = newCodomain }


view : Model -> Html Msg
view model =
    let
        proportions =
            countProportions <| countFunctions model
    in
    div []
        [ inlineStyle
        , slider "Domain size" ChangeDomain model.domain
        , slider "Codomain size" ChangeCodomain model.codomain
        , countsTable proportions
        , proportionDiagram proportions
        ]


countsTable : Proportions -> Html a
countsTable { noInjNoSur, noInjYesSur, yesInjNoSur, yesInjYesSur } =
    let
        textCell t =
            td [] [ text t ]

        intCell =
            textCell << String.fromInt
    in
    table []
        [ tr []
            [ textCell "Injective? / Surjective?"
            , textCell "No"
            , textCell "Yes"
            , textCell "Total"
            ]
        , tr []
            [ textCell "No"
            , intCell noInjNoSur
            , intCell noInjYesSur
            , intCell <| noInjNoSur + noInjYesSur
            ]
        , tr []
            [ textCell "Yes"
            , intCell yesInjNoSur
            , intCell yesInjYesSur
            , intCell <| yesInjNoSur + yesInjYesSur
            ]
        , tr []
            [ textCell "Total"
            , intCell <| noInjNoSur + yesInjNoSur
            , intCell <| noInjYesSur + yesInjYesSur
            , intCell <| noInjNoSur + noInjYesSur + yesInjNoSur + yesInjYesSur
            ]
        ]


proportionDiagram : Proportions -> Html a
proportionDiagram { noInjNoSur, noInjYesSur, yesInjNoSur, yesInjYesSur } =
    let
        totalSum =
            noInjNoSur + noInjYesSur + yesInjNoSur + yesInjYesSur

        proportionSquare =
            if noInjNoSur < 0 || noInjYesSur < 0 || yesInjNoSur < 0 || yesInjYesSur < 0 || totalSum == 0 then
                Svg.text_ [ x "200", y "200" ] [ Svg.text "No data to draw diagram" ]

            else
                let
                    noInjNoSur_proportion =
                        toFloat noInjNoSur / toFloat totalSum

                    noInjYesSur_proportion =
                        toFloat noInjYesSur / toFloat totalSum

                    yesInjNoSur_proportion =
                        toFloat yesInjNoSur / toFloat totalSum

                    yesInjYesSur_proportion =
                        toFloat yesInjYesSur / toFloat totalSum
                in
                Svg.g []
                    [ Svg.rect
                        [ x (String.fromFloat <| 200 - 200 * noInjNoSur_proportion)
                        , y (String.fromFloat <| 200 - 200 * noInjNoSur_proportion)
                        , width <| round <| 200 * noInjNoSur_proportion
                        , height <| round <| 200 * noInjNoSur_proportion
                        , fill "rgb(239,41,41)" --lightRed
                        ]
                        []
                    , Svg.text_ [ x "20", y "20" ] [ Svg.text <| String.fromFloat noInjNoSur_proportion ]
                    , Svg.rect
                        [ x "200"
                        , y (String.fromFloat <| 200 - 200 * noInjYesSur_proportion)
                        , width <| round <| 200 * noInjYesSur_proportion
                        , height <| round <| 200 * noInjYesSur_proportion
                        , fill "rgb(138,226,52)" --lightGreen
                        ]
                        []
                    , Svg.text_ [ x "220", y "20" ] [ Svg.text <| String.fromFloat noInjYesSur_proportion ]
                    , Svg.rect
                        [ x (String.fromFloat <| 200 - 200 * yesInjNoSur_proportion)
                        , y "200"
                        , width <| round <| 200 * yesInjNoSur_proportion
                        , height <| round <| 200 * yesInjNoSur_proportion
                        , fill "rgb(114,159,207)" --lightBlue
                        ]
                        []
                    , Svg.text_ [ x "20", y "220" ] [ Svg.text <| String.fromFloat yesInjNoSur_proportion ]
                    , Svg.rect
                        [ x "200"
                        , y "200"
                        , width <| round <| 200 * yesInjYesSur_proportion
                        , height <| round <| 200 * yesInjYesSur_proportion
                        , fill "rgb(252,175,62)" -- lightOrange
                        ]
                        []
                    , Svg.text_ [ x "220", y "220" ] [ Svg.text <| String.fromFloat yesInjYesSur_proportion ]
                    ]
    in
    Svg.svg [ width 400, height 400 ] [ proportionSquare ]


inlineStyle : Html a
inlineStyle =
    Html.node "style" [] [ Html.text "table{border-collapse:collapse;}table,th,td{border:1px solid black;}" ]


slider : String -> (String -> Msg) -> Int -> Html Msg
slider lbl msg val =
    div []
        [ label []
            [ text lbl
            , input [ type_ "range", Attr.min "0", Attr.max "10", step "1", onInput msg, value (String.fromInt val) ] []
            , text (String.fromInt val)
            ]
        ]
