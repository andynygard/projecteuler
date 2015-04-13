collatz 1 = [1]
collatz x = [x] ++ (collatz . next $ x)
    where
        next y
            | even y    = y `div` 2
            | otherwise = y * 3 + 1

main = print . maximum . map (\x -> (length . collatz $ x, x)) $ [1..1000000]
