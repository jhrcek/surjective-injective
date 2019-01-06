module Main where
import Control.Applicative
import Data.List
import Math.NumberTheory.Recurrencies.Bilinear (stirling2)
import Math.NumberTheory.Recurrencies.Linear (factorial)
import Text.Printf

main :: IO ()
main = do
  let xs = formatLine <$> liftA2 (,) [0..10] [0..10]
  putStrLn $ "[" <> intercalate ",\n" xs <> "\n]"

formatLine :: (Int, Int) -> String
formatLine (a,b) =
    printf "(%s, { noInjNoSur = %d, noInjYesSur = %d, yesInjNoSur = %d, yesInjYesSur = %d})" (show (a,b)) noInjNoSur noInjYesSur yesInjNoSur yesInjYesSur
  where
    noInjYesSur = countSurNotInj a b
    yesInjYesSur = countBij a b
    yesInjNoSur = countInjNotSur a b
    noInjNoSur = countAll a b - noInjYesSur - yesInjYesSur - yesInjNoSur

countSurNotInj :: Int -> Int -> Integer
countSurNotInj a b
    | a < 0 || b < 0 || a <= b = 0
    | otherwise = (factorial !! b) * (stirling2memo !! a !! b)

countInjNotSur :: Int -> Int -> Integer
countInjNotSur a b
    | a < 0 || b < 0 || a >= b = 0
    | otherwise = let b' = fromIntegral b in product $ take a [b', pred b' ..]

countBij :: Int -> Int -> Integer
countBij a b
    | a == b = factorial !! a
    | otherwise = 0

-- Making the function monomorphic to take advantages of memoization
-- as suggested in https://www.stackage.org/haddock/lts-13.1/arithmoi-0.8.0.0/Math-NumberTheory-Recurrencies-Bilinear.html
stirling2memo :: [[Integer]]
stirling2memo = stirling2

{-| Count all functions A -> B, where |A|=a and |B|=b -}
countAll :: Int -> Int -> Integer
countAll a b = fromIntegral b ^ a
