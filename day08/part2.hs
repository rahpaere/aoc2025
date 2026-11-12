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

connectAll :: Circuits -> [(Point, Point)] -> (Circuits, [(Point, Point)])
connectAll circuits [] = (circuits, [])
connectAll circuits (connection:connections) =
    if (length $ components circuits) == 1
    then (circuits, [])
    else
        (connected, (connection:conns))
        where (connected, conns) = connectAll (connectCircuits circuits connection) connections

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

components :: Circuits -> [Int]
components = map fst . Map.elems . Map.filter (\(_, parent) -> parent == Nothing)

main :: IO ()
main = do
    points <- map parseCoords <$> lines <$> getContents
    let connections = map (\(_, p, q) -> (p, q)) $ distances points
        initCircuits = makeCircuits points
        (finalCircuits, conns) = connectAll initCircuits connections
        sizes = components finalCircuits
        ((x1, _, _), (x2, _, _)) = last conns
    print $ x1 * x2
