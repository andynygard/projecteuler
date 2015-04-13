dataFile = "13_data.txt"

digitsOfSum n ns = (take n) . show . sum $ ns

io f = do
    s <- readFile dataFile
    let nums = map read (lines s)
    putStr . f $ nums

main = io . digitsOfSum $ 10
