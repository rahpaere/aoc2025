import Data.List
import Data.List.Split
import Debug.Trace

parseOperands :: String -> [[Int]]
parseOperands = map (map read . lines) . splitOn "\n\n" . filter (/= ' ')

calculate' :: String -> [Int] -> Int
calculate' op
    | op == "*" = product
    | op == "+" = sum
    | otherwise = error "operation must be * or +"

main :: IO ()
main = do
    input <- lines <$> getContents
    let operands = parseOperands . unlines . reverse . transpose . init $ input
        operations = reverse . words . last $ input
    print . sum $ zipWith calculate' operations operands
