import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int, Int)
type Circuits = Map Point (Int, Maybe Point)

makeCircuits :: [Point] -> Circuits
makeCircuits points = Map.fromList [(p, (1, Nothing)) | p <- points]

findCircuit :: Point -> Circuits -> (Int, Point)
findCircuit p circuits =
    case Map.lookup p circuits of
    Nothing -> error "point not found"
    Just (size, Nothing) -> (size, p)
    Just (_, Just parent) -> findCircuit parent circuits

connectCircuits :: Circuits -> (Point, Point) -> Circuits
connectCircuits circuits (p, q) =
    if pCircuit == qCircuit
    then circuits
    else Map.adjust (const (qSize, Just p)) qCircuit $
            Map.adjust (const (size, Nothing)) pCircuit circuits
    where
        (pSize, pCircuit) = findCircuit p circuits
        (qSize, qCircuit) = findCircuit q circuits
        size = pSize + qSize

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ dx^^2 + dy^^2 + dz^^2
    where
        dx = fromIntegral $ x1 - x2
        dy = fromIntegral $ y1 - y2
        dz = fromIntegral $ z1 - z2

parseCoords :: String -> Point
parseCoords s = read $ concat ["(", s, ")"]

compareDistances :: (Double, Point, Point) -> (Double, Point, Point) -> Ordering
compareDistances (d1, _, _) (d2, _, _) = compare d1 d2

distances :: [Point] -> [(Double, Point, Point)]
distances points = sortBy compareDistances ds
    where ds = [(distance p q, p, q) | p <- points, q <- points, p < q]

main :: IO ()
main = do
    points <- map parseCoords <$> lines <$> getContents
    let connections = take 1000 . map (\(_, p, q) -> (p, q)) $ distances points
        initCircuits = makeCircuits points
        finalCircuits = foldl connectCircuits initCircuits connections
        sizes = map fst . Map.elems . Map.filter (\(_, parent) -> parent == Nothing) $ finalCircuits
        largest = take 3 $ sortBy (flip compare) sizes
    print $ product largest
