import Data.Map

data Element = Hydrophobic | Polar
  deriving (Eq, Show)

type Pos = Integer

type Position = (Pos, Pos)

type ProteinMap = Map Position Element

isHydrophobic :: ProteinMap -> Position -> Bool
isHydrophobic placements position = (Data.Map.lookup position placements) == Just Hydrophobic

neighbours :: Position -> [Position]
neighbours (x, y) = [(x + x1, y + y1) | (x1,y1) <- [(0,1), (1,0), (0,-1), (-1,0)]]

countBonds :: ProteinMap -> Position -> Integer
countBonds placements position = if isHydrophobic placements position
  then (toInteger . length . Prelude.filter (\x -> isHydrophobic placements x) . neighbours) position
  else 0

search :: ProteinMap -> [Element] -> Integer
search placements [] = 0
search placements (next:rest) = 0

optimise :: [Element] -> Integer
optimise (p1:p2:protein) =
    search placements protein
  where 
    placements = fromList [((0,0), p1), ((0, 1), p2)] :: ProteinMap

main = do
  putStrLn "Project Euler 300"
