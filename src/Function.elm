module Function exposing
    ( Function
    , FunctionCounts
    , FunctionCountsRelative
    , SetSizes
    , eval
    , identityFunction
    , lookupCounts
    , randomFunctionGen
    , toRelativeCounts
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.Array


type alias SetSizes =
    { domain : Int
    , codomain : Int
    }


type alias FunctionCounts =
    { noInjNoSur : Int
    , noInjYesSur : Int
    , yesInjNoSur : Int
    , yesInjYesSur : Int
    }


type alias FunctionCountsRelative =
    { noInjNoSur : Float
    , noInjYesSur : Float
    , yesInjNoSur : Float
    , yesInjYesSur : Float
    }


toRelativeCounts : FunctionCounts -> Maybe FunctionCountsRelative
toRelativeCounts { noInjNoSur, noInjYesSur, yesInjNoSur, yesInjYesSur } =
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


{-| Represents function from {1..setSizes.domain} to {1..setSizes.codomain}
-}
type Function
    = Function (Array Int)



--| TODO shouldn't generate anything if function of given type doesn't exist


randomFunctionGen : SetSizes -> Generator Function
randomFunctionGen setSizes =
    Random.Array.array setSizes.domain (Random.int 1 setSizes.codomain)
        |> Random.map Function


eval : Function -> List ( Int, Int )
eval (Function f) =
    List.map (Tuple.mapFirst (\x -> x + 1)) <| Array.toIndexedList f


identityFunction : Int -> Function
identityFunction n =
    Function (Array.initialize n (\x -> x + 1))


lookupCounts : SetSizes -> FunctionCounts
lookupCounts setSizes =
    Dict.get ( setSizes.domain, setSizes.codomain ) countsDict
        |> Maybe.withDefault (FunctionCounts 0 0 0 0)


countsDict : Dict ( Int, Int ) FunctionCounts
countsDict =
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
