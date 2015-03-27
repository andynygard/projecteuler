import Data.List

triangles = scanl1 (+) [1..]

numDivisors a = foldl' countDivisors 0 [1..(floor . sqrt . fromIntegral $ a)]
    where
        countDivisors x y = x + if a `mod` y == 0 then 2 else 0

main = print . find (\x -> (numDivisors x) > 500) $ triangles
