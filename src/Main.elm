module Main exposing (main)

import Browser
import Element exposing (Attribute, Element, px, rgb, rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import FormatNumber
import FormatNumber.Locales exposing (Locale)
import FunctionCounts exposing (FunctionCounts)
import Html exposing (Html, text)
import Html.Attributes exposing (height, width)
import Svg.Attributes exposing (fill)


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
    | ChangeDomainAndCodomain Int Int
    | ChangePercentPrecision Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDomain newDomain ->
            { model | domain = newDomain }

        ChangeCodomain newCodomain ->
            { model | codomain = newCodomain }

        ChangePercentPrecision newPrecision ->
            { model | percentPrecision = clamp 0 10 newPrecision }

        ChangeDomainAndCodomain newDomain newCodomain ->
            { model | domain = newDomain, codomain = newCodomain }


view : Model -> Html Msg
view model =
    let
        fcounts =
            FunctionCounts.lookupCounts ( model.domain, model.codomain )
    in
    Element.layout [] <|
        Element.column []
            [ slider 10 (5 * cellSize) "Domain size" ChangeDomain model.domain
            , slider 10 (5 * cellSize) "Codomain size" ChangeCodomain model.codomain
            , proportionsTable model
            , countsTable fcounts
            , slider 5 75 "Percent precision" ChangePercentPrecision model.percentPrecision
            ]


proportionsTable : Model -> Element Msg
proportionsTable model =
    codomainSizeLegendRow
        :: List.map
            (\rowIdx ->
                numberCell rowIdx
                    :: List.map
                        (\colIdx ->
                            let
                                isCellSelected =
                                    model.domain == rowIdx && model.codomain == colIdx
                            in
                            previewCell rowIdx colIdx isCellSelected
                        )
                        (List.range 0 10)
                    |> Element.row []
            )
            (List.range 0 10)
        |> Element.column
            [ Border.color (rgb 0 0 0)
            , Border.solid
            , Border.width 1
            ]


previewCell : Int -> Int -> Bool -> Element Msg
previewCell rowIdx colIdx isCellSelected =
    let
        fcounts =
            FunctionCounts.lookupCounts ( rowIdx, colIdx )

        alpha =
            if isCellSelected then
                1

            else
                0.3

        cellContents =
            case FunctionCounts.toRelative fcounts of
                Nothing ->
                    [ Element.el [ Element.centerX, Element.centerY ] (Element.text "None") ]

                Just _ ->
                    [ Element.el
                        [ Background.color (rgba 1 0 0 alpha)
                        , Element.height (Element.fillPortion fcounts.noInjYesSur)
                        , Element.width Element.fill
                        ]
                        Element.none
                    , Element.el
                        [ Background.color (rgba 0 1 0 alpha)
                        , Element.height (Element.fillPortion fcounts.yesInjYesSur)
                        , Element.width Element.fill
                        ]
                        Element.none
                    , Element.el
                        [ Background.color (rgba 0 0 1 alpha)
                        , Element.height (Element.fillPortion fcounts.yesInjNoSur)
                        , Element.width Element.fill
                        ]
                        Element.none
                    , Element.el
                        [ Background.color (rgba 0.5 0.5 0.5 alpha)
                        , Element.height (Element.fillPortion fcounts.noInjNoSur)
                        , Element.width Element.fill
                        ]
                        Element.none
                    ]
    in
    Element.column
        (Events.onClick (ChangeDomainAndCodomain rowIdx colIdx) :: commonCellAttributes)
        cellContents


commonCellAttributes : List (Attribute msg)
commonCellAttributes =
    [ Element.width (px cellSize)
    , Element.height (px cellSize)
    , Border.color (rgb 0 0 0)
    , Border.solid
    , Border.width 1
    ]


cellSize : Int
cellSize =
    60


numberCell : Int -> Element msg
numberCell n =
    Element.el commonCellAttributes <|
        Element.el
            [ Element.centerX, Element.centerY ]
            (Element.text <| String.fromInt n)


codomainSizeLegendRow : Element msg
codomainSizeLegendRow =
    (Element.el commonCellAttributes Element.none
        :: List.map (\colIdx -> numberCell colIdx) (List.range 0 10)
    )
        |> Element.row []


countsTable : FunctionCounts -> Element msg
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
