module Main exposing (main)

import Browser
import Element exposing (Element, px, rgb, rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import FormatNumber
import FormatNumber.Locales exposing (Locale)
import Function exposing (Function, FunctionCountsRelative, SetSizes, eval, randomFunctionGen)
import Html exposing (Html)
import Random
import Svg exposing (Svg, circle, g, line, path, rect, svg)
import Svg.Attributes exposing (cx, cy, d, fill, fillOpacity, height, r, rx, ry, stroke, strokeWidth, transform, width, x, x1, x2, y, y1, y2)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { setSizes : SetSizes
    , percentPrecision : Int
    , randomFunction : Function
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { setSizes =
            { domain = 1
            , codomain = 1
            }
      , percentPrecision = 1
      , randomFunction = Function.identityFunction 1
      }
    , Cmd.none
    )


type Msg
    = ChangeDomain Int
    | ChangeCodomain Int
    | ChangeDomainAndCodomain SetSizes
    | ChangePercentPrecision Int
    | GenerateRandomFunction
    | ReceivedRandomFunction Function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDomain domain ->
            let
                newSetSize =
                    { domain = domain
                    , codomain = model.setSizes.codomain
                    }
            in
            ( { model | setSizes = newSetSize }
            , generateRandomFunction model.setSizes
            )

        ChangeCodomain codomain ->
            let
                newSetSizes =
                    { domain = model.setSizes.domain
                    , codomain = codomain
                    }
            in
            ( { model | setSizes = newSetSizes }
            , generateRandomFunction newSetSizes
            )

        ChangePercentPrecision percentPrecision ->
            ( { model | percentPrecision = clamp 0 10 percentPrecision }
            , Cmd.none
            )

        ChangeDomainAndCodomain setSizes ->
            ( { model | setSizes = setSizes }
            , generateRandomFunction setSizes
            )

        GenerateRandomFunction ->
            ( model
            , generateRandomFunction model.setSizes
            )

        ReceivedRandomFunction randomFunction ->
            ( { model | randomFunction = randomFunction }, Cmd.none )


generateRandomFunction : SetSizes -> Cmd Msg
generateRandomFunction setSizes =
    Random.generate ReceivedRandomFunction (randomFunctionGen setSizes)


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column []
            [ slider 10 (5 * cellSize) "Domain size" ChangeDomain model.setSizes.domain
            , slider 10 (5 * cellSize) "Codomain size" ChangeCodomain model.setSizes.codomain
            , Element.row []
                [ proportionsTable model.setSizes
                , proportionsView model.setSizes
                , Element.column [ Element.alignTop ]
                    [ infoTable model.setSizes model.percentPrecision
                    , slider 5 75 "Percent precision" ChangePercentPrecision model.percentPrecision
                    , functionDiagram model.setSizes model.randomFunction
                    , randomFunctionGeneratorControls
                    ]
                ]
            ]


randomFunctionGeneratorControls : Element Msg
randomFunctionGeneratorControls =
    Input.button []
        { onPress = Just GenerateRandomFunction
        , label = Element.text "Generate"
        }


functionDiagram : SetSizes -> Function -> Element msg
functionDiagram setSizes f =
    let
        setView setSize =
            rect [ x "1", y "1", width "60", height (String.fromInt <| 45 * max 1 setSize + 15), rx "30", ry "30", stroke "black", fill "none", strokeWidth "2" ] []
                :: (List.range 1 setSize
                        |> List.map (\i -> circle [ cx "30", cy (circleYCoord i), r "15", fill "white", stroke "black", strokeWidth "2" ] [])
                   )

        circleYCoord idx =
            String.fromInt <| 45 * idx - 15

        domainView =
            setView setSizes.domain

        codomainView =
            g [ transform "translate(300,0)" ] <| setView setSizes.codomain

        mappingLines =
            g [] <| List.map (\( x, y ) -> line [ x1 "30", x2 "330", y1 (circleYCoord x), y2 (circleYCoord y), stroke "black", strokeWidth "2" ] []) <| Function.eval f
    in
    Element.html <|
        svg [ width "500", height "500" ]
            (mappingLines :: codomainView :: domainView)


proportionsTable : SetSizes -> Element Msg
proportionsTable setSizes =
    codomainSizeLegendRow
        :: List.map
            (\rowIdx ->
                numberCell rowIdx
                    :: List.map
                        (\colIdx ->
                            let
                                isCellSelected =
                                    setSizes.domain == rowIdx && setSizes.codomain == colIdx
                            in
                            previewCell { domain = rowIdx, codomain = colIdx } isCellSelected
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


previewCell : SetSizes -> Bool -> Element Msg
previewCell setSizes isCellSelected =
    let
        fcounts =
            Function.lookupCounts setSizes

        alpha =
            if isCellSelected then
                1

            else
                0.5

        cellContents =
            case Function.toRelativeCounts fcounts of
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
        (Events.onClick (ChangeDomainAndCodomain setSizes) :: commonCellAttributes)
        cellContents


commonCellAttributes : List (Element.Attribute msg)
commonCellAttributes =
    [ Element.width (px cellSize)
    , Element.height (px cellSize)
    , Border.color (rgb 0 0 0)
    , Border.solid
    , Border.width 1
    ]


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


infoTable : SetSizes -> Int -> Element msg
infoTable setSizes percentPrecision =
    let
        fcounts =
            Function.lookupCounts setSizes

        relCounts =
            Function.toRelativeCounts fcounts |> Maybe.withDefault (FunctionCountsRelative 0 0 0 0)

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
              , view = \fi -> coloredCell fi.color (formatAsPercent percentPrecision <| fi.relativeCount)
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


proportionsView : SetSizes -> Element msg
proportionsView setSizes =
    let
        emptyView =
            Element.el [ Element.width (px mappingImageWidth) ] <| Element.text "No functions"
    in
    Function.lookupCounts setSizes
        |> Function.toRelativeCounts
        |> Maybe.map (proportionsToTableMapping >> Element.html)
        |> Maybe.withDefault emptyView


proportionsToTableMapping : FunctionCountsRelative -> Svg msg
proportionsToTableMapping relCounts =
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
        [ width <| String.fromInt mappingImageWidth
        , height <| String.fromFloat svgHeight
        ]
        [ rect [ width "50", height (String.fromFloat greenHeight), x "0", y "0", fill "lime" ] []
        , rect [ width "50", height (String.fromFloat blueHeight), x "0", y (String.fromFloat greenEnd), fill "blue" ] []
        , rect [ width "50", height (String.fromFloat redHeight), x "0", y (String.fromFloat blueEnd), fill "red" ] []
        , rect [ width "50", height (String.fromFloat grayHeight), x "0", y (String.fromFloat redEnd), fill "gray" ] []
        , path [ d <| "M 50 0                                   L 150 30  v 30 L 50 " ++ String.fromFloat greenEnd ++ "Z", fill "lime", fillOpacity "0.5" ] []
        , path [ d <| "M 50 " ++ String.fromFloat greenEnd ++ " L 150 60  v 30 L 50 " ++ String.fromFloat blueEnd ++ "Z", fill "blue", fillOpacity "0.5" ] []
        , path [ d <| "M 50 " ++ String.fromFloat blueEnd ++ "  L 150 90  v 30 L 50 " ++ String.fromFloat redEnd ++ "Z", fill "red", fillOpacity "0.5" ] []
        , path [ d <| "M 50 " ++ String.fromFloat redEnd ++ "   L 150 120 v 30 L 50 " ++ String.fromFloat grayEnd ++ "Z", fill "grey", fillOpacity "0.5" ] []
        ]


{-| Size of cell in the Proportions table
-}
cellSize : Int
cellSize =
    60


{-| Width of the svg image mapping proportions stacked bar to the info table
-}
mappingImageWidth : Int
mappingImageWidth =
    150
