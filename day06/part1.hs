import Data.List
import Debug.Trace
import Text.Read

calculate :: [String] -> Int
calculate problem = calculate' [] problem
    where
        calculate' :: [Int] -> [String] -> Int
        calculate' terms ["*"] = product terms
        calculate' terms ["+"] = sum terms
        calculate' terms [_] = error "expected operation"
        calculate' terms (n:ns) = calculate' (terms ++ [read n]) ns

main :: IO ()
main = do
    problems <- transpose . map words . lines <$> getContents
    print . sum . map calculate $ problems
