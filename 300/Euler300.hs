import Prelude hiding (lookup)
import Data.Map (Map, lookup, foldrWithKey, insert, fromList)
import Data.Maybe
import Control.Monad
import System.IO.Unsafe

data Element = Hydrophobic | Polar
  deriving (Eq, Ord, Show)

type Pos = Integer

type Position = (Pos, Pos)

type ProteinMap = Map Position Element

fromChar :: Char -> Element
fromChar 'H' = Hydrophobic
fromChar 'P' = Polar

toChar :: Element -> Char
toChar Hydrophobic = 'H'
toChar Polar = 'P'

fromString :: String -> [Element]
fromString = map fromChar

toString :: [Element] -> String
toString = map toChar

isFree :: ProteinMap -> Position -> Bool
isFree placements position = (lookup position placements) == Nothing

isHydrophobic :: ProteinMap -> Position -> Bool
isHydrophobic placements position = (lookup position placements) == Just Hydrophobic

neighbours :: Position -> [Position]
neighbours (x, y) = [(x + x1, y + y1) | (x1,y1) <- [(0,1), (1,0), (0,-1), (-1,0)]]

countBonds :: ProteinMap -> Position -> Integer
countBonds placements position = if isHydrophobic placements position
  then (toInteger . length . filter (\x -> isHydrophobic placements x) . neighbours) position
  else 0

getBounds :: ProteinMap -> (Pos, Pos, Pos, Pos)
getBounds = foldrWithKey (\(x,y) _ (x1,y1,x2,y2) -> (min x x1, min y y1, max x x2, max y y2)) (0,0,0,0)

printPlacements :: ProteinMap -> IO [()]
printPlacements placements =
  let (minX,minY,maxX,maxY) = getBounds placements in
    forM [minY..maxY] (\y -> printRow y [minX..maxX])
  where
    printRow y xs = putStrLn $ show y ++ " " ++ map (\x -> showPosition $ lookup (x, y) placements) xs
    showPosition Nothing = ' '
    showPosition (Just x) = toChar x

search :: ProteinMap -> Position -> [Element] -> Maybe Integer
--search placements position [] = (unsafePerformIO $ do {printPlacements placements; putStrLn ""; return (Just 0) })
search placements position [] = (Just 0)
search placements position (next:rest) = foldr choose Nothing (map searchNeighbour freeNeighbours)
  where
    freeNeighbours :: [Position]
    freeNeighbours = filter (isFree placements) (neighbours position)
    searchNeighbour :: Position -> Maybe Integer
    searchNeighbour n =
      (liftM (placedBonds +)) (search newPlacements n rest)
      where
        newPlacements = insert n next placements
        placedBonds = countBonds newPlacements n
    choose :: Maybe Integer -> Maybe Integer -> Maybe Integer
    choose Nothing b = b
    choose (Just a) Nothing = Just a
    choose (Just a) (Just b) = Just (max a b)

optimise :: [Element] -> Integer
optimise (p1:p2:protein) =
    fromJust $ (liftM ((countBonds placements (0,1)) +)) (search placements (0,1) protein)
  where 
    placements = fromList [((0,0), p1), ((0, 1), p2)] :: ProteinMap

generateStrings :: Integer -> [a] -> [[a]]
generateStrings 0 _ = [[]]
generateStrings n l = [x:l2 | l2 <- generateStrings (n-1) l, x <- l]

-- Perform a weighted optimisation of the string; if the string is a palandrome, then
-- the weight is 1.  If the string comes before its reversal, then the weight is 2.  
-- But if the string comes after its reversal then the weight is 0 and the optimisation
-- does not need to be calculated!  (The score would be the same as the reversed string's
-- which gets the weight 2.)
weightedOptimise :: [Element] -> Integer
weightedOptimise x = case (compare x (reverse x)) of
  LT -> 2 * (optimise x)
  EQ -> optimise x
  GT -> 0

main :: IO Integer
main = do
  putStrLn "Project Euler 300"
  counts <- forM (generateStrings 12 [Hydrophobic, Polar]) (\x -> do
    --putStrLn $ (toString x) ++ " " ++ (show $ weightedOptimise x)
    return $ weightedOptimise x
    )
  putStrLn $ "Number: " ++ (show . length) counts
  putStrLn $ "Total: " ++ (show . sum) counts
  putStrLn $ "Average: " ++ show( (fromIntegral $ sum counts) / (fromIntegral $ length counts))
  return $ sum counts
