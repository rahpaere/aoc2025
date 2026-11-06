comboZeroes :: [Int] -> Int
comboZeroes turns = length . filter (== 0) . map (`mod` 100) $ scanl (+) 50 turns

parseTurn :: String -> Int
parseTurn ('L':s) = -(read s)
parseTurn ('R':s) = read s
parseTurn _ = error "must turn L or R"

main :: IO ()
main = do
    zeroes <- comboZeroes . map parseTurn . lines <$> getContents
    print zeroes
