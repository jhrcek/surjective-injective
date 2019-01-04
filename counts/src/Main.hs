module Main where
import Math.NumberTheory.Recurrencies.Bilinear (stirling2)
import Math.NumberTheory.Recurrencies.Linear (factorial)

main :: IO ()
main = do
  print $ countSurjective 3 2

countSurjective :: Int -> Int -> Integer
countSurjective a b =
    (factorial !! b) * (stirling2 !! a !! b)
