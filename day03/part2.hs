import Data.Char

parseInput :: String -> [[Int]]
parseInput s = map (map digitToInt) $ lines s

incorporate :: Int -> [Int] -> [Int]
incorporate b [] = []
incorporate b bs@(h:t) = if b >= h then b:(incorporate h t) else bs

bestBatteries :: Int -> [Int] -> [Int]
bestBatteries n bs@(b:t) = if length bs <= n then bs else incorporate b (bestBatteries n t)

joltage :: [Int] -> Int
joltage [] = 0
joltage bs = joltage' 0 bs
    where
        joltage' n [] = n
        joltage' n (b:bs) = joltage' (n * 10 + b) bs

bestJoltage :: Int -> [Int] -> Int
bestJoltage n bs = joltage $ bestBatteries n bs

main :: IO ()
main = do
    banks <- parseInput <$> getContents
    print $ sum . map (bestJoltage 12) $ banks
