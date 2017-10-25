module Main exposing (..)

import Count
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { domain : Int
    , codomain : Int
    }


type Msg
    = ChangeDomain String
    | ChangeCodomain String


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
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
                    String.toInt d |> Result.withDefault 0
            in
            { model | domain = newDomain }

        ChangeCodomain c ->
            let
                newCodomain =
                    String.toInt c |> Result.withDefault 0
            in
            { model | codomain = newCodomain }


view : Model -> Html Msg
view { domain, codomain } =
    let
        total =
            Count.allFunctions domain codomain

        sur =
            Count.surjective domain codomain

        inj =
            Count.injective domain codomain

        bij =
            Count.bijective domain codomain
    in
    div []
        [ inlineStyle
        , slider "Domain size" ChangeDomain domain
        , slider "Codomain size" ChangeCodomain codomain
        , table []
            [ tr []
                [ td [] [ text "Injective? / Surjective?" ]
                , td [] [ text "No" ]
                , td [] [ text "Yes" ]
                , td [] [ text "Total" ]
                ]
            , tr []
                [ td [] [ text "No" ]
                , td [] [ text <| toString <| total - sur - inj + bij ]
                , td [] [ text <| toString <| sur - bij ]
                , td [] [ text <| toString <| total - inj ]
                ]
            , tr []
                [ td [] [ text "Yes" ]
                , td [] [ text <| toString <| inj - bij ]
                , td [] [ text <| toString <| bij ]
                , td [] [ text <| toString <| inj ]
                ]
            , tr []
                [ td [] [ text "Total" ]
                , td [] [ text <| toString <| total - sur ]
                , td [] [ text <| toString sur ]
                , td [] [ text <| toString total ]
                ]
            ]
        ]


inlineStyle : Html a
inlineStyle =
    Html.node "style" [] [ Html.text "table {border-collapse: collapse;}table, th, td {border: 1px solid black;}" ]


slider : String -> (String -> Msg) -> Int -> Html Msg
slider lbl msg val =
    div []
        [ label []
            [ text lbl
            , input [ type_ "range", Attr.min "0", Attr.max "10", step "1", onInput msg, value (toString val) ] []
            , text (toString val)
            ]
        ]
