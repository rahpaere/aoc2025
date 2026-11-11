import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

splits :: Set Int -> Set Int -> (Int, Set Int)
splits beams splitters = (length activated, nextBeams)
    where
        activated = Set.intersection beams splitters
        continued = Set.difference beams activated
        lefts = Set.map (+1) activated
        rights = Set.map (subtract 1) activated
        created = Set.union lefts rights
        nextBeams = Set.union continued created

manifoldSplits :: Set Int -> [Set Int] -> Int
manifoldSplits _ [] = 0
manifoldSplits beams (row:rows) = n + manifoldSplits nextBeams rows
    where (n, nextBeams) = splits beams row

main :: IO ()
main = do
    start <- Set.fromList . elemIndices 'S' <$> getLine
    manifold <- map (Set.fromList . elemIndices '^') . lines <$> getContents
    print $ manifoldSplits start manifold
