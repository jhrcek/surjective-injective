module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, px, rgb)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FormatNumber
import FormatNumber.Locales exposing (Locale)
import Html exposing (Html, text)
import Html.Attributes exposing (height, width)
import Svg
import Svg.Attributes exposing (fill, x, y)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { domain : Int
    , codomain : Int
    , percentPrecision : Int
    }


init : Model
init =
    { domain = 1
    , codomain = 1
    , percentPrecision = 1
    }


type Msg
    = ChangeDomain Int
    | ChangeCodomain Int
    | ChangePercentPrecision Int


type alias Proportions =
    { noInjNoSur : Int
    , noInjYesSur : Int
    , yesInjNoSur : Int
    , yesInjYesSur : Int
    }


type alias ProportionsRelative =
    { noInjNoSur : Float
    , noInjYesSur : Float
    , yesInjNoSur : Float
    , yesInjYesSur : Float
    }


toRelative : Proportions -> Maybe ProportionsRelative
toRelative { noInjNoSur, noInjYesSur, yesInjNoSur, yesInjYesSur } =
    let
        totalSum =
            noInjNoSur + noInjYesSur + yesInjNoSur + yesInjYesSur
    in
    if noInjNoSur < 0 || noInjYesSur < 0 || yesInjNoSur < 0 || yesInjYesSur < 0 || totalSum == 0 then
        Nothing

    else
        Just
            { noInjNoSur = toFloat noInjNoSur / toFloat totalSum
            , noInjYesSur = toFloat noInjYesSur / toFloat totalSum
            , yesInjNoSur = toFloat yesInjNoSur / toFloat totalSum
            , yesInjYesSur = toFloat yesInjYesSur / toFloat totalSum
            }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDomain newDomain ->
            { model | domain = newDomain }

        ChangeCodomain newCodomain ->
            { model | codomain = newCodomain }

        ChangePercentPrecision newPrecision ->
            { model | percentPrecision = clamp 0 10 newPrecision }


view : Model -> Html Msg
view model =
    let
        proportions =
            Dict.get ( model.domain, model.codomain ) proportionsDict
                |> Maybe.withDefault (Proportions -1 -1 -1 -1)
    in
    Element.layout [] <|
        Element.column []
            [ slider 10 150 "Domain size" ChangeDomain model.domain
            , slider 10 150 "Codomain size" ChangeCodomain model.codomain
            , slider 5 75 "PercentPrecision" ChangePercentPrecision model.percentPrecision
            , countsTable proportions
            , Element.html <| proportionDiagram proportions model.percentPrecision
            ]


countsTable : Proportions -> Element a
countsTable { noInjNoSur, noInjYesSur, yesInjNoSur, yesInjYesSur } =
    Element.table []
        { data =
            [ Rec "No" noInjNoSur yesInjNoSur (noInjNoSur + yesInjNoSur)
            , Rec "Yes" noInjYesSur yesInjYesSur (noInjYesSur + yesInjYesSur)
            , Rec "Total" (noInjNoSur + noInjYesSur) (yesInjNoSur + yesInjYesSur) (noInjNoSur + noInjYesSur + yesInjNoSur + yesInjYesSur)
            ]
        , columns =
            [ { header = Element.text "Injective? / Surjective?", width = Element.px 250, view = \{ s } -> Element.text s }
            , { header = Element.text "No", width = Element.px 150, view = \{ x } -> Element.text <| String.fromInt x }
            , { header = Element.text "Yes", width = Element.px 100, view = \{ y } -> Element.text <| String.fromInt y }
            , { header = Element.text "Total", width = Element.px 100, view = \{ z } -> Element.text <| String.fromInt z }
            ]
        }


type alias Rec =
    { s : String
    , x : Int
    , y : Int
    , z : Int
    }


proportionDiagram : Proportions -> Int -> Html a
proportionDiagram proportions precision =
    let
        formatPercent =
            formatAsPercent precision

        proportionsView =
            case toRelative proportions of
                Nothing ->
                    Svg.text_ [ x "100", y "200" ] [ Svg.text "No data to draw diagram" ]

                Just { noInjNoSur, noInjYesSur, yesInjNoSur, yesInjYesSur } ->
                    Svg.g []
                        [ Svg.rect
                            [ x (String.fromFloat <| 200 - 200 * noInjNoSur)
                            , y (String.fromFloat <| 200 - 200 * noInjNoSur)
                            , width <| round <| 200 * noInjNoSur
                            , height <| round <| 200 * noInjNoSur
                            , fill "rgb(239,41,41)" --lightRed
                            ]
                            []
                        , Svg.text_ [ x "20", y "20" ] [ Svg.text <| formatPercent noInjNoSur ]
                        , Svg.rect
                            [ x "200"
                            , y (String.fromFloat <| 200 - 200 * noInjYesSur)
                            , width <| round <| 200 * noInjYesSur
                            , height <| round <| 200 * noInjYesSur
                            , fill "rgb(138,226,52)" --lightGreen
                            ]
                            []
                        , Svg.text_ [ x "220", y "20" ] [ Svg.text <| formatPercent noInjYesSur ]
                        , Svg.rect
                            [ x (String.fromFloat <| 200 - 200 * yesInjNoSur)
                            , y "200"
                            , width <| round <| 200 * yesInjNoSur
                            , height <| round <| 200 * yesInjNoSur
                            , fill "rgb(114,159,207)" --lightBlue
                            ]
                            []
                        , Svg.text_ [ x "20", y "220" ] [ Svg.text <| formatPercent yesInjNoSur ]
                        , Svg.rect
                            [ x "200"
                            , y "200"
                            , width <| round <| 200 * yesInjYesSur
                            , height <| round <| 200 * yesInjYesSur
                            , fill "rgb(252,175,62)" -- lightOrange
                            ]
                            []
                        , Svg.text_ [ x "220", y "220" ] [ Svg.text <| formatPercent yesInjYesSur ]
                        ]
    in
    Svg.svg [ width 400, height 400 ] [ proportionsView ]


slider : Int -> Int -> String -> (Int -> Msg) -> Int -> Element Msg
slider maxVal w lbl toMsg val =
    Input.slider
        [ Background.color (rgb 0.8 0.8 0.8)
        , Border.width 1
        , Border.rounded 7
        , Element.width (px w)
        , Element.height (px 6)
        ]
        { onChange = toMsg << round
        , label = Input.labelRight [] <| Element.text <| lbl ++ " = " ++ String.fromInt val
        , min = 0
        , max = toFloat maxVal
        , value = toFloat val
        , thumb = Input.defaultThumb
        , step = Just 1
        }


formatAsPercent : Int -> Float -> String
formatAsPercent decimalPrecision x =
    FormatNumber.format (locale decimalPrecision) (x * 100) ++ "%"


locale : Int -> Locale
locale decimalPrecision =
    Locale decimalPrecision "," "." "−" "" "" ""


proportionsDict : Dict ( Int, Int ) Proportions
proportionsDict =
    Dict.fromList
        [ ( ( 0, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 1 } )
        , ( ( 0, 1 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 2 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 3 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 4 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 5 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 6 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 7 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 8 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 9 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 0, 10 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 1, yesInjYesSur = 0 } )
        , ( ( 1, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 1, 1 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 1 } )
        , ( ( 1, 2 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 2, yesInjYesSur = 0 } )
        , ( ( 1, 3 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 3, yesInjYesSur = 0 } )
        , ( ( 1, 4 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 4, yesInjYesSur = 0 } )
        , ( ( 1, 5 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 5, yesInjYesSur = 0 } )
        , ( ( 1, 6 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 6, yesInjYesSur = 0 } )
        , ( ( 1, 7 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 7, yesInjYesSur = 0 } )
        , ( ( 1, 8 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 8, yesInjYesSur = 0 } )
        , ( ( 1, 9 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 9, yesInjYesSur = 0 } )
        , ( ( 1, 10 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 10, yesInjYesSur = 0 } )
        , ( ( 2, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 2, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 2, 2 ), { noInjNoSur = 2, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 2 } )
        , ( ( 2, 3 ), { noInjNoSur = 3, noInjYesSur = 0, yesInjNoSur = 6, yesInjYesSur = 0 } )
        , ( ( 2, 4 ), { noInjNoSur = 4, noInjYesSur = 0, yesInjNoSur = 12, yesInjYesSur = 0 } )
        , ( ( 2, 5 ), { noInjNoSur = 5, noInjYesSur = 0, yesInjNoSur = 20, yesInjYesSur = 0 } )
        , ( ( 2, 6 ), { noInjNoSur = 6, noInjYesSur = 0, yesInjNoSur = 30, yesInjYesSur = 0 } )
        , ( ( 2, 7 ), { noInjNoSur = 7, noInjYesSur = 0, yesInjNoSur = 42, yesInjYesSur = 0 } )
        , ( ( 2, 8 ), { noInjNoSur = 8, noInjYesSur = 0, yesInjNoSur = 56, yesInjYesSur = 0 } )
        , ( ( 2, 9 ), { noInjNoSur = 9, noInjYesSur = 0, yesInjNoSur = 72, yesInjYesSur = 0 } )
        , ( ( 2, 10 ), { noInjNoSur = 10, noInjYesSur = 0, yesInjNoSur = 90, yesInjYesSur = 0 } )
        , ( ( 3, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 3, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 3, 2 ), { noInjNoSur = 2, noInjYesSur = 6, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 3, 3 ), { noInjNoSur = 21, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 6 } )
        , ( ( 3, 4 ), { noInjNoSur = 40, noInjYesSur = 0, yesInjNoSur = 24, yesInjYesSur = 0 } )
        , ( ( 3, 5 ), { noInjNoSur = 65, noInjYesSur = 0, yesInjNoSur = 60, yesInjYesSur = 0 } )
        , ( ( 3, 6 ), { noInjNoSur = 96, noInjYesSur = 0, yesInjNoSur = 120, yesInjYesSur = 0 } )
        , ( ( 3, 7 ), { noInjNoSur = 133, noInjYesSur = 0, yesInjNoSur = 210, yesInjYesSur = 0 } )
        , ( ( 3, 8 ), { noInjNoSur = 176, noInjYesSur = 0, yesInjNoSur = 336, yesInjYesSur = 0 } )
        , ( ( 3, 9 ), { noInjNoSur = 225, noInjYesSur = 0, yesInjNoSur = 504, yesInjYesSur = 0 } )
        , ( ( 3, 10 ), { noInjNoSur = 280, noInjYesSur = 0, yesInjNoSur = 720, yesInjYesSur = 0 } )
        , ( ( 4, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 4, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 4, 2 ), { noInjNoSur = 2, noInjYesSur = 14, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 4, 3 ), { noInjNoSur = 45, noInjYesSur = 36, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 4, 4 ), { noInjNoSur = 232, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 24 } )
        , ( ( 4, 5 ), { noInjNoSur = 505, noInjYesSur = 0, yesInjNoSur = 120, yesInjYesSur = 0 } )
        , ( ( 4, 6 ), { noInjNoSur = 936, noInjYesSur = 0, yesInjNoSur = 360, yesInjYesSur = 0 } )
        , ( ( 4, 7 ), { noInjNoSur = 1561, noInjYesSur = 0, yesInjNoSur = 840, yesInjYesSur = 0 } )
        , ( ( 4, 8 ), { noInjNoSur = 2416, noInjYesSur = 0, yesInjNoSur = 1680, yesInjYesSur = 0 } )
        , ( ( 4, 9 ), { noInjNoSur = 3537, noInjYesSur = 0, yesInjNoSur = 3024, yesInjYesSur = 0 } )
        , ( ( 4, 10 ), { noInjNoSur = 4960, noInjYesSur = 0, yesInjNoSur = 5040, yesInjYesSur = 0 } )
        , ( ( 5, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 5, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 5, 2 ), { noInjNoSur = 2, noInjYesSur = 30, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 5, 3 ), { noInjNoSur = 93, noInjYesSur = 150, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 5, 4 ), { noInjNoSur = 784, noInjYesSur = 240, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 5, 5 ), { noInjNoSur = 3005, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 120 } )
        , ( ( 5, 6 ), { noInjNoSur = 7056, noInjYesSur = 0, yesInjNoSur = 720, yesInjYesSur = 0 } )
        , ( ( 5, 7 ), { noInjNoSur = 14287, noInjYesSur = 0, yesInjNoSur = 2520, yesInjYesSur = 0 } )
        , ( ( 5, 8 ), { noInjNoSur = 26048, noInjYesSur = 0, yesInjNoSur = 6720, yesInjYesSur = 0 } )
        , ( ( 5, 9 ), { noInjNoSur = 43929, noInjYesSur = 0, yesInjNoSur = 15120, yesInjYesSur = 0 } )
        , ( ( 5, 10 ), { noInjNoSur = 69760, noInjYesSur = 0, yesInjNoSur = 30240, yesInjYesSur = 0 } )
        , ( ( 6, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 6, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 6, 2 ), { noInjNoSur = 2, noInjYesSur = 62, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 6, 3 ), { noInjNoSur = 189, noInjYesSur = 540, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 6, 4 ), { noInjNoSur = 2536, noInjYesSur = 1560, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 6, 5 ), { noInjNoSur = 13825, noInjYesSur = 1800, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 6, 6 ), { noInjNoSur = 45936, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 720 } )
        , ( ( 6, 7 ), { noInjNoSur = 112609, noInjYesSur = 0, yesInjNoSur = 5040, yesInjYesSur = 0 } )
        , ( ( 6, 8 ), { noInjNoSur = 241984, noInjYesSur = 0, yesInjNoSur = 20160, yesInjYesSur = 0 } )
        , ( ( 6, 9 ), { noInjNoSur = 470961, noInjYesSur = 0, yesInjNoSur = 60480, yesInjYesSur = 0 } )
        , ( ( 6, 10 ), { noInjNoSur = 848800, noInjYesSur = 0, yesInjNoSur = 151200, yesInjYesSur = 0 } )
        , ( ( 7, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 2 ), { noInjNoSur = 2, noInjYesSur = 126, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 3 ), { noInjNoSur = 381, noInjYesSur = 1806, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 4 ), { noInjNoSur = 7984, noInjYesSur = 8400, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 5 ), { noInjNoSur = 61325, noInjYesSur = 16800, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 6 ), { noInjNoSur = 264816, noInjYesSur = 15120, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 7, 7 ), { noInjNoSur = 818503, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 5040 } )
        , ( ( 7, 8 ), { noInjNoSur = 2056832, noInjYesSur = 0, yesInjNoSur = 40320, yesInjYesSur = 0 } )
        , ( ( 7, 9 ), { noInjNoSur = 4601529, noInjYesSur = 0, yesInjNoSur = 181440, yesInjYesSur = 0 } )
        , ( ( 7, 10 ), { noInjNoSur = 9395200, noInjYesSur = 0, yesInjNoSur = 604800, yesInjYesSur = 0 } )
        , ( ( 8, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 2 ), { noInjNoSur = 2, noInjYesSur = 254, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 3 ), { noInjNoSur = 765, noInjYesSur = 5796, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 4 ), { noInjNoSur = 24712, noInjYesSur = 40824, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 5 ), { noInjNoSur = 264625, noInjYesSur = 126000, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 6 ), { noInjNoSur = 1488096, noInjYesSur = 191520, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 7 ), { noInjNoSur = 5623681, noInjYesSur = 141120, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 8, 8 ), { noInjNoSur = 16736896, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 40320 } )
        , ( ( 8, 9 ), { noInjNoSur = 42683841, noInjYesSur = 0, yesInjNoSur = 362880, yesInjYesSur = 0 } )
        , ( ( 8, 10 ), { noInjNoSur = 98185600, noInjYesSur = 0, yesInjNoSur = 1814400, yesInjYesSur = 0 } )
        , ( ( 9, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 2 ), { noInjNoSur = 2, noInjYesSur = 510, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 3 ), { noInjNoSur = 1533, noInjYesSur = 18150, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 4 ), { noInjNoSur = 75664, noInjYesSur = 186480, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 5 ), { noInjNoSur = 1119005, noInjYesSur = 834120, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 6 ), { noInjNoSur = 8172576, noInjYesSur = 1905120, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 7 ), { noInjNoSur = 38025127, noInjYesSur = 2328480, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 8 ), { noInjNoSur = 132766208, noInjYesSur = 1451520, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 9, 9 ), { noInjNoSur = 387057609, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 362880 } )
        , ( ( 9, 10 ), { noInjNoSur = 996371200, noInjYesSur = 0, yesInjNoSur = 3628800, yesInjYesSur = 0 } )
        , ( ( 10, 0 ), { noInjNoSur = 0, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 1 ), { noInjNoSur = 0, noInjYesSur = 1, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 2 ), { noInjNoSur = 2, noInjYesSur = 1022, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 3 ), { noInjNoSur = 3069, noInjYesSur = 55980, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 4 ), { noInjNoSur = 230056, noInjYesSur = 818520, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 5 ), { noInjNoSur = 4662625, noInjYesSur = 5103000, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 6 ), { noInjNoSur = 44030736, noInjYesSur = 16435440, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 7 ), { noInjNoSur = 252840049, noInjYesSur = 29635200, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 8 ), { noInjNoSur = 1043501824, noInjYesSur = 30240000, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 9 ), { noInjNoSur = 3470454801, noInjYesSur = 16329600, yesInjNoSur = 0, yesInjYesSur = 0 } )
        , ( ( 10, 10 ), { noInjNoSur = 9996371200, noInjYesSur = 0, yesInjNoSur = 0, yesInjYesSur = 3628800 } )
        ]
