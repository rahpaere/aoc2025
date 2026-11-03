parseTurn :: String -> Int
parseTurn ('L':s) = -(read s)
parseTurn ('R':s) = read s

zeroes :: Int -> Int -> Int
zeroes cur clicks =
    let (q, r) = quotRem clicks 100
        last = cur + r > 99 || cur + r <= 0 && cur > 0
    in (abs q) + (fromEnum last)

addZeroes :: (Int, Int) -> Int -> (Int, Int)
addZeroes (total, cur) clicks =
    (total + zeroes cur clicks, (cur + clicks) `mod` 100)

main :: IO ()
main = do
    contents <- getContents
    let turns = map parseTurn $ lines contents
    print . fst $ foldl addZeroes (0, 50) turns
