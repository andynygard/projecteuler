import Data.List

evenDivBy r = getLowest $ reverse . sort $ r
    where
        getLowest (x:xs) = head $ filter isEvenDiv $ map (x *) [1..]
            where
                isEvenDiv y = all (\z -> y `mod` z == 0) xs

main = print $ evenDivBy [2..20]
