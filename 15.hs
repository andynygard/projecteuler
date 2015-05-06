snake n m = recSnake (0,0)
    where
        recSnake (a,b)
            | a < n && b < m    = recSnake (a + 1,b) + recSnake (a,b + 1)
            | a < n             = recSnake (a + 1,b)
            | b < m             = recSnake (a,b + 1)
            | otherwise         = 1 

main = print $ snake 20 20
