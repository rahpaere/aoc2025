import Data.Set (Set)
import qualified Data.Set as Set

colCoords :: (Int, Int) -> [Char] -> [(Int, Int)]
colCoords _ [] = []
colCoords coord@(row, col) (x:xs) =
    if x == '@'
    then coord:rest
    else rest
    where rest = colCoords (row, col + 1) xs

rowCoords :: Int -> [[Char]] -> [[(Int, Int)]]
rowCoords _ [] = []
rowCoords rowNum (row:rows) =
    (colCoords (rowNum, 0) row):(rowCoords (rowNum + 1) rows)

parseDiagram :: [[Char]] -> Set (Int, Int)
parseDiagram = Set.fromAscList . concat . rowCoords 0

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (row, col) = 
    [(row + dR, col + dC) | dR <- [-1, 0 ,1], dC <- [-1, 0, 1], (dR, dC) /= (0, 0)]

countNeighbors :: Set (Int, Int) -> (Int, Int) -> Int
countNeighbors locs loc =
    length $ filter (`Set.member` locs) (neighbors loc)

reachable :: Set (Int, Int) -> (Int, Int) -> Bool
reachable locs loc = countNeighbors locs loc < 4

countReachable :: Set (Int, Int) -> Int
countReachable locs = length . Set.filter (reachable locs) $ locs

main :: IO ()
main = do
    contents <- lines <$> getContents
    print $ countReachable . parseDiagram $ contents
