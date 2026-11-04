import Control.Lens
import Data.List.Split

makeId :: Int -> Int
makeId n = f 1
    where f p = if p > n then n * p + n else f (p * 10)

minInvalidHalf :: String -> Int
minInvalidHalf s =
    let half = (length s) `div` 2
        (top, bot) = over both (read . ('0':)) $ splitAt half s
        actual = top + (fromEnum $ bot > top)
        digits = (length s) - half
        wideEnough = 10 ^ (digits - 1)
    in max actual wideEnough

maxInvalidHalf :: String -> Int
maxInvalidHalf s =
    let half = (length s) `div` 2
        (top, bot) = over both read $ splitAt half s
        actual = top - (fromEnum $ bot < top)
        digits = (length s) - half
        wideEnough = 10 ^ (digits - 1) - 1
    in max actual wideEnough

invalidRange :: String -> (Int, Int)
invalidRange s =
    let bounds = splitOn "-" s
    in
        case bounds of
        [a, b] -> (minInvalidHalf a, maxInvalidHalf b)
        _ -> error "could not split on -"

sumRange :: (Int, Int) -> Int
sumRange (a, b) = sum $ map makeId [a..b]

main :: IO ()
main = do
    contents <- getContents
    let line = concat . lines $ contents
    print $ sum $ map (sumRange . invalidRange) $ splitOn "," line
