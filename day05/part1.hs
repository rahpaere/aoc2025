import Control.Lens
import Data.List.Split

parseRange :: String -> (Int, Int)
parseRange s = (lo, hi)
    where [lo, hi] = map read $ splitOn "-" s

inRange :: (Int, Int) -> Int -> Bool
inRange (lo, hi) n = lo <= n && n <= hi

inRanges :: [(Int, Int)] -> Int -> Bool
inRanges ranges n = any (`inRange` n) ranges

checkRanges :: [(Int, Int)] -> [Int] -> [Int]
checkRanges ranges ns = filter (inRanges ranges) ns

main :: IO ()
main = do
    parts <- map lines . splitOn "\n\n" <$> getContents
    let [fstPart, sndPart] = parts
        ranges :: [(Int, Int)] = map parseRange fstPart
        values :: [Int] = map read sndPart
    print $ length $ checkRanges ranges values
