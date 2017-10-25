module Count
    exposing
        ( allFunctions
        , bijective
        , injective
        , surjective
        )

import List


allFunctions : Int -> Int -> Int
allFunctions domainSize codomainSize =
    codomainSize ^ domainSize


bijective : Int -> Int -> Int
bijective domainSize codomainSize =
    if domainSize /= codomainSize then
        0
    else
        List.product <| List.range 1 domainSize


injective : Int -> Int -> Int
injective domainSize codomainSize =
    List.product <| List.range (codomainSize - domainSize + 1) codomainSize


surjective : Int -> Int -> Int
surjective n k =
    if k == 0 && n == 0 then
        1
    else if k < 1 then
        0
    else if k > n then
        0
    else
        List.sum <| List.map (\i -> negateIfOdd (k - i) <| binomial k i * i ^ n) <| List.range 0 k


negateIfOdd : Int -> Int -> Int
negateIfOdd x y =
    if x % 2 == 1 then
        -y
    else
        y


binomial : Int -> Int -> Int
binomial n k =
    if k > n || k < 0 then
        0
    else if k > (n // 2) then
        binomial n (n - k)
    else
        (List.product <| List.range (n - k + 1) n) // (List.product <| List.range 1 k)
