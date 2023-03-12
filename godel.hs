import Control.Monad
import Text.Printf
import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.Primes.Sieve


-- Encode a list of (non-negative) integers as a Godel number
godelEncode :: [Integer] -> Integer
godelEncode = merge primes
  where
    -- merge a list of ascending prime numbers and a list of integers, raising each prime p
    -- to the power of its corresponding integer, and returning the product.
    merge _ [] = 1
    merge (p:ps) (n:ns) = (p^n)*(merge ps ns)

-- Decode a Godel number into a list of integers
godelDecode :: Integer -> [Integer]
godelDecode = merge primes . factorise
  where
    -- merge a list of ascending prime numbers and a list of (prime factor, exponent) tuples,
    -- generating a new list of the exponents with 0s as placeholders where prime factors
    -- were absent.
    merge :: [Integer] -> [(Integer, Int)] -> [Integer]
    merge _ [] = []
    merge (p:ps) ((f, n):fs)
      | f == p = (toInteger n):merge ps fs
      | otherwise = 0:merge ps ((f, n):fs)

-- Print some example Godel numbers and what they represent
main :: IO ()
main = do
  putStrLn "Decoded Godel numbers"
  forM [1..1000] printExample
  return ()
  where
    printExample x = do
      let xdec = godelDecode x
      let xdecenc = godelEncode xdec
      putStrLn $ printf "%d -> %s <- %d" x (showList xdec) x 
    showList x
      | length x > 50 = (show $ take 50 x) ++ "..."
      | otherwise = show x
