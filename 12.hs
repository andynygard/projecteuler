import Data.List

triangles = scanl1 (+) [1..]

divisors a = filter (\x -> a `mod` x == 0) [1..a]

main = print . find (\x -> (length . divisors $ x) > 500) $ triangles
