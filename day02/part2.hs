import Data.List
import Data.List.Split

makeId' :: Int -> Int -> Int
makeId' reps n = repDigits (nextPower 1) reps 0
    where
        nextPower p = if p > n then p else nextPower $ p * 10
        repDigits p r t = if r == 0 then t else repDigits p (r - 1) (t * p + n)

splitChunks :: String -> Int -> Int -> [Int]
splitChunks s n reps = map read $ chunksOf n $ pad s ((n * reps) - (length s))
    where
        pad s' count = if count == 0 then s' else pad ('0':s') (count - 1)

-- Least chunk that would make an invalid ID not less than the input.
lexCeil :: [Int] -> Int
lexCeil (n:rest) = lexCeil' n rest
    where
        lexCeil' n' [] = n'
        lexCeil' n' (x:xs)
            | x < n' = n'
            | x > n' = n' + 1
            | otherwise = lexCeil' n' xs
lexCeil _ = error "must have nonempty list"

-- Greatest chunk that would make an invalid ID not greater than the input.
lexFloor :: [Int] -> Int
lexFloor (n:rest) = lexFloor' n rest
    where
        lexFloor' n' [] = n'
        lexFloor' n' (x:xs)
            | x < n' = n' - 1
            | x > n' = n'
            | otherwise = lexFloor' n' xs
lexFloor _ = error "must have nonempty list"

chunkCeil :: String -> Int -> Int
chunkCeil s reps = max actual wideEnough
    where
        n = ((length s) + reps - 1) `quot` reps
        chunks = splitChunks s n reps
        actual = lexCeil chunks
        wideEnough = 10 ^ (n - 1)

chunkFloor :: String -> Int -> Int
chunkFloor s reps = max actual wideEnough
    where
        n = ((length s) + reps - 1) `quot` reps
        chunks = splitChunks s n reps
        actual = lexFloor chunks
        wideEnough = 10 ^ (n - 1) - 1

chunkRange :: String -> String -> Int -> [Int]
chunkRange a b reps = map (makeId' reps) [start..stop]
    where
        start = chunkCeil a reps
        stop = chunkFloor b reps

invalidRange :: String -> (String, String)
invalidRange s =
    let bounds = splitOn "-" s
    in
        case bounds of
        [a, b] -> (a, b)
        _ -> error "could not split on -"

sumRange' :: (String, String) -> Int
sumRange' (a, b) = sum $ nub $ concat $ map (chunkRange a b) [2..m]
    where m = max (length a) (length b)

main :: IO ()
main = do
    contents <- getContents
    let ranges = splitOn "," $ concat . lines $ contents
    print $ sum $ map (sumRange' . invalidRange) ranges
