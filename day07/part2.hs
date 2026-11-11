import Data.List

mergeBeams :: [(Int, Int)] -> [(Int, Int)]
mergeBeams [] = []
mergeBeams [x] = [x]
mergeBeams ((aCol, aN):(bCol, bN):more)
    | aCol == bCol = mergeBeams ((aCol, aN + bN):more)
    | otherwise = (aCol, aN):(mergeBeams ((bCol, bN):more))

splitBeams :: [(Int, Int)] -> [Int] -> [(Int, Int)]
splitBeams [] _ = []
splitBeams rest [] = rest
splitBeams field@((col, n):beams) splitters@(splitCol:splits)
    | col == splitCol = (col - 1, n):(col + 1, n):splitBeams beams splits
    | col < splitCol = (col, n):splitBeams beams splitters
    | col > splitCol = splitBeams field splits

runManifoldRow :: [(Int, Int)] -> [Int] -> [(Int, Int)]
runManifoldRow row splitters = mergeBeams $ splitBeams row splitters

runManifold :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
runManifold start manifold = foldl runManifoldRow start manifold

main :: IO ()
main = do
    startLine <- getLine
    manifoldLines <- lines <$> getContents
    let start = [(x, 1) | x <- elemIndices 'S' startLine]
        manifold = map (elemIndices '^') manifoldLines
    print . sum . map snd $ runManifold start manifold
