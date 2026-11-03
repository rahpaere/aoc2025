comboZeroes :: [Int] -> Int
comboZeroes turns = length . filter (== 0) . map (`mod` 100) $ scanl (+) 50 turns

parseTurn :: String -> Int
parseTurn ('L':s) = -(read s)
parseTurn ('R':s) = read s

main :: IO ()
main = do
    contents <- getContents
    let turns = map parseTurn $ lines contents
    print $ comboZeroes turns
