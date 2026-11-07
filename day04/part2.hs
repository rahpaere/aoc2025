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

pruneReachable :: Set (Int, Int) -> Set (Int, Int)
pruneReachable locs = Set.filter (not . reachable locs) locs

findFixed :: Eq a => (a -> a) -> a -> a
findFixed f x = if x == x' then x else findFixed f x'
    where x' = f x

pruned :: Set (Int, Int) -> Set (Int, Int)
pruned = findFixed pruneReachable

main :: IO ()
main = do
    locs <- parseDiagram <$> lines <$> getContents
    let locs' = pruned locs
    print $ length locs - length locs'
