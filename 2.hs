main = print . sum . filter even . takeWhile (< 4000000) $ fib

fib = 1 : inner 1 1
    where
        inner x y = (x + y) : (inner y (x + y)) 
