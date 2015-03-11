import Data.Array

dataFile = "11_data.txt"

calcGrid :: [[Int]] -> Int
calcGrid g = g!!0!!1 -- Select some random cell for now to test with

io f = do
    s <- readFile dataFile
    let rows = map (map read . words) . lines $ s
    putStr . show . f $ rows

main = io $ calcGrid
