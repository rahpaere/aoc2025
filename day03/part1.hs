import Data.Char

parseInput :: String -> [[Int]]
parseInput s = map (map digitToInt) $ lines s

bestJoltage :: [Int] -> Int
bestJoltage batteries = 10 * tens + ones
    where
        tens = maximum . init $ batteries
        ones = maximum . tail $ dropWhile (/= tens) batteries

main :: IO ()
main = do
    banks <- parseInput <$> getContents
    print $ sum . map bestJoltage $ banks
