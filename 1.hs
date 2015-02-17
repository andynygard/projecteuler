import Data.List (union)

main = print . sum $ union (multiples 3) (multiples 5)
    where
        multiples a = takeWhile (< 1000) $ map (* a) [1..]
