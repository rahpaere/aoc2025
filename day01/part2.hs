parseTurn :: String -> Int
parseTurn ('L':s) = -(read s)
parseTurn ('R':s) = read s
parseTurn _ = error "must turn L or R"

zeroes :: Int -> Int -> Int
zeroes cur clicks =
    let (q, r) = quotRem clicks 100
        finalZero = cur + r > 99 || cur + r <= 0 && cur > 0
    in (abs q) + (fromEnum finalZero)

addZeroes :: (Int, Int) -> Int -> (Int, Int)
addZeroes (total, cur) clicks =
    (total + zeroes cur clicks, (cur + clicks) `mod` 100)

main :: IO ()
main = do
    turns <- map parseTurn . lines <$> getContents
    print . fst $ foldl addZeroes (0, 50) turns
