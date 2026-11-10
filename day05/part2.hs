import Control.Lens
import Data.List.Split
import Data.List

parseRange :: String -> (Int, Int)
parseRange s = (lo, hi)
    where [lo, hi] = map read $ splitOn "-" s

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = merge' [] where
    merge' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge' as [] = as
    merge' [] (b:bs) = merge' [b] bs
    merge' (a@(aLo, aHi):as) (b@(bLo, bHi):bs)
        | aHi + 1 < bLo = a:(merge' as (b:bs))
        | bHi + 1 < aLo = b:(merge' (a:as) bs)
        | otherwise = merge' as ((min aLo bLo, max aHi bHi):bs)

rangeLength :: (Int, Int) -> Int
rangeLength (lo, hi) = hi - lo + 1

main :: IO ()
main = do
    parts <- map lines . splitOn "\n\n" <$> getContents
    let ranges = map parseRange $ head parts
        merged = mergeRanges $ sort ranges
    print . sum . map rangeLength $ merged
