module Main exposing (main)

import Browser
import Element exposing (Element, px, rgb, rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import FormatNumber
import FormatNumber.Locales exposing (Locale)
import FunctionCounts exposing (FunctionCountsRelative)
import Html exposing (Html)
import Svg exposing (circle, g, path, rect, svg)
import Svg.Attributes exposing (cx, cy, d, fill, fillOpacity, height, r, rx, ry, stroke, strokeWidth, transform, width, x, y)


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
    Element.layout [] <|
        Element.column []
            [ slider 10 (5 * cellSize) "Domain size" ChangeDomain model.domain
            , slider 10 (5 * cellSize) "Codomain size" ChangeCodomain model.codomain
            , Element.row []
                [ proportionsTable model
                , proportionsView model
                , Element.column [ Element.alignTop ]
                    [ infoTable model
                    , slider 5 75 "Percent precision" ChangePercentPrecision model.percentPrecision
                    , functionDiagram model
                    ]
                ]
            ]


functionDiagram : Model -> Element msg
functionDiagram model =
    let
        setView setSize =
            rect [ x "1", y "1", width "60", height (String.fromInt <| 45 * max 1 setSize + 15), rx "30", ry "30", stroke "black", fill "none", strokeWidth "2" ] []
                :: (List.range 1 setSize
                        |> List.map (\i -> circle [ cx "30", cy (String.fromInt <| 45 * i - 15), r "15", fill "none", stroke "black", strokeWidth "2" ] [])
                   )

        domainView =
            setView model.domain

        codomainView =
            g [ transform "translate(300,0)" ] <| setView model.codomain
    in
    Element.html <|
        svg [ width "500", height "500" ]
            (codomainView :: domainView)


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
                0.5

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


commonCellAttributes : List (Element.Attribute msg)
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


infoTable : Model -> Element msg
infoTable model =
    let
        fcounts =
            FunctionCounts.lookupCounts ( model.domain, model.codomain )

        relCounts =
            FunctionCounts.toRelative fcounts |> Maybe.withDefault (FunctionCountsRelative 0 0 0 0)

        solidBlackBorder =
            [ Border.solid, Border.width 1 ]

        cell =
            Element.el (Element.padding 4 :: solidBlackBorder) << Element.text

        coloredCell color =
            Element.el (Background.color color :: Element.padding 4 :: solidBlackBorder) << Element.text
    in
    Element.table
        solidBlackBorder
        { data =
            [ FunctionInfo (rgba 0 1 0 0.5) "Yes" "Yes" fcounts.yesInjYesSur relCounts.yesInjYesSur
            , FunctionInfo (rgba 0 0 1 0.5) "Yes" "No" fcounts.yesInjNoSur relCounts.yesInjNoSur
            , FunctionInfo (rgba 1 0 0 0.5) "No" "Yes" fcounts.noInjYesSur relCounts.noInjYesSur
            , FunctionInfo (rgba 0.5 0.5 0.5 0.5) "No" "No" fcounts.noInjNoSur relCounts.noInjNoSur
            , FunctionInfo (rgba 0 0 0 0)
                "?"
                "?"
                (fcounts.yesInjYesSur + fcounts.yesInjNoSur + fcounts.noInjYesSur + fcounts.noInjNoSur)
                (relCounts.yesInjYesSur + relCounts.yesInjNoSur + relCounts.noInjYesSur + relCounts.noInjNoSur)
            ]
        , columns =
            [ { header = cell "Injective?"
              , width = Element.fill
              , view = \fi -> coloredCell fi.color fi.injInfo
              }
            , { header = cell "Surjective?"
              , width = Element.fill
              , view = \fi -> coloredCell fi.color fi.surInfo
              }
            , { header = cell "Absolute count"
              , width = Element.fill
              , view = \fi -> coloredCell fi.color (String.fromInt fi.absoluteCount)
              }
            , { header = cell "Relative count"
              , width = Element.fill
              , view = \fi -> coloredCell fi.color (formatAsPercent model.percentPrecision <| fi.relativeCount)
              }
            ]
        }


type alias FunctionInfo =
    { color : Element.Color
    , injInfo : String
    , surInfo : String
    , absoluteCount : Int
    , relativeCount : Float
    }


proportionsView : Model -> Element msg
proportionsView model =
    let
        adapterWidth =
            150

        viewAdapter relCounts =
            let
                svgHeight =
                    toFloat (12 * cellSize)

                -- Relative height of each color
                greenHeight =
                    svgHeight * relCounts.yesInjYesSur

                blueHeight =
                    svgHeight * relCounts.yesInjNoSur

                redHeight =
                    svgHeight * relCounts.noInjYesSur

                grayHeight =
                    svgHeight * relCounts.noInjNoSur

                -- Cumulative height
                greenEnd =
                    greenHeight

                blueEnd =
                    greenEnd + blueHeight

                redEnd =
                    blueEnd + redHeight

                grayEnd =
                    redEnd + grayHeight
            in
            svg
                [ width <| String.fromInt adapterWidth
                , height <| String.fromFloat svgHeight
                ]
                [ -- Green = bijective
                  rect [ x "0", y "0", height (String.fromFloat greenHeight), fill "lime", width "48" ] []
                , path [ d <| "M 50 0  L 150 30  v 30 L 50 " ++ String.fromFloat greenEnd ++ "Z", fill "lime", fillOpacity "0.5" ] []

                -- Blue = injective, not surjective
                , rect [ x "0", y (String.fromFloat greenEnd), height (String.fromFloat blueHeight), fill "blue", width "48" ] []
                , path [ d <| "M 50 " ++ String.fromFloat greenEnd ++ "  L 150 60  v 30 L 50 " ++ String.fromFloat blueEnd ++ "Z", fill "blue", fillOpacity "0.5" ] []

                -- Red = surjective, not injective
                , rect [ x "0", y (String.fromFloat blueEnd), height (String.fromFloat redHeight), fill "red", width "48" ] []
                , path [ d <| "M 50 " ++ String.fromFloat blueEnd ++ "  L 150 90  v 30 L 50 " ++ String.fromFloat redEnd ++ "Z", fill "red", fillOpacity "0.5" ] []

                -- Gray = Not surjective, not injective
                , rect [ x "0", y (String.fromFloat redEnd), height (String.fromFloat grayHeight), fill "gray", width "48" ] []
                , path [ d <| "M 50 " ++ String.fromFloat redEnd ++ "  L 150 120  v 30 L 50 " ++ String.fromFloat grayEnd ++ "Z", fill "grey", fillOpacity "0.5" ] []
                ]

        emptyView =
            Element.el [ Element.width (px adapterWidth) ] <| Element.text "No functions"
    in
    FunctionCounts.lookupCounts ( model.domain, model.codomain )
        |> FunctionCounts.toRelative
        |> Maybe.map (viewAdapter >> Element.html)
        |> Maybe.withDefault emptyView
