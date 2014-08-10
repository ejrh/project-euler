{--module Euler451 (
  Natural,
  isSelfInverse,
  largestSelfInverse,
  main
  ) where--}

import Text.Printf
import System.IO.Unsafe

type Natural = Int

isSelfInverse :: Natural -> Natural -> Bool
isSelfInverse m n = (gcd m n == 1) && (1 == ((toInteger m) * (toInteger m)) `mod` (toInteger n))

largestSelfInverse :: Natural -> Natural
largestSelfInverse n = searchDown [n-2,n-3..1] n
  where
    searchDown :: [Natural] -> Natural -> Natural
    searchDown [] n = 0
    searchDown (m:ms) n
      | isSelfInverse m n = m
      | otherwise = searchDown ms n 

main = do
  putStrLn "Project Euler 451"
  putStrLn $ show $ map (\x -> (x,largestSelfInverse x)) [1..20]
  printf "l(100) = 51; got %d\n" (largestSelfInverse 100)
  printf "l(7) = 1; got %d\n" (largestSelfInverse 7)
  let lsi2m = (largestSelfInverse 2000000)
  printf "l(2000000) = ?; got %d\n" lsi2m
  putStrLn $ show $ foldl (\acc x -> acc + largestSelfInverse (write x)) lsi2m [2*10^7-1,2*10^7-2..3]
  where
    write :: Natural -> Natural
    write x
      | x `mod` 10 == 0 = unsafePerformIO $ do { putStrLn $ show x; return x}
      | otherwise = x
