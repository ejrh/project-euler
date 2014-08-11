import Data.Map
import Control.Monad
import System.IO.Unsafe

data Element = Hydrophobic | Polar
  deriving (Eq, Show)

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
fromString = Prelude.map fromChar

isFree :: ProteinMap -> Position -> Bool
isFree placements position = (Data.Map.lookup position placements) == Nothing

isHydrophobic :: ProteinMap -> Position -> Bool
isHydrophobic placements position = (Data.Map.lookup position placements) == Just Hydrophobic

neighbours :: Position -> [Position]
neighbours (x, y) = [(x + x1, y + y1) | (x1,y1) <- [(0,1), (1,0), (0,-1), (-1,0)]]

countBonds :: ProteinMap -> Position -> Integer
countBonds placements position = if isHydrophobic placements position
  then (toInteger . length . Prelude.filter (\x -> isHydrophobic placements x) . neighbours) position
  else 0

getBounds :: ProteinMap -> (Pos, Pos, Pos, Pos)
getBounds = foldrWithKey (\(x,y) _ (x1,y1,x2,y2) -> (min x x1, min y y1, max x x2, max y y2)) (0,0,0,0)

printPlacements :: ProteinMap -> IO [()]
printPlacements placements =
  let (minX,minY,maxX,maxY) = getBounds placements in
    forM [minY..maxY] (\y -> printRow y [minX..maxX])
  where
    printRow y xs = putStrLn $ show y ++ " " ++ Prelude.map (\x -> showPosition $ Data.Map.lookup (x, y) placements) xs
    showPosition Nothing = ' '
    showPosition (Just x) = toChar x

search :: ProteinMap -> Position -> [Element] -> Maybe Integer
search placements position [] = (unsafePerformIO $ do {printPlacements placements; putStrLn ""; return (Just 0) })
search placements position (next:rest) = Prelude.foldr choose Nothing (Prelude.map searchNeighbour freeNeighbours)
  where
    freeNeighbours :: [Position]
    freeNeighbours = Prelude.filter (isFree placements) (neighbours position)
    searchNeighbour n =
      (liftM (placedBonds +)) (search newPlacements n rest)
      where
        newPlacements = insert n next placements
        placedBonds = countBonds newPlacements n
    choose :: Maybe Integer -> Maybe Integer -> Maybe Integer
    choose Nothing b = b
    choose (Just a) Nothing = Just a
    choose (Just a) (Just b) = Just (max a b)

optimise :: [Element] -> Maybe Integer
optimise (p1:p2:protein) =
    (liftM ((countBonds placements (0,1)) +)) (search placements (0,1) protein)
  where 
    placements = fromList [((0,0), p1), ((0, 1), p2)] :: ProteinMap

main = do
  putStrLn "Project Euler 300"
  let test = fromString "HPPH"
  putStrLn $ show $ optimise test
