import Data.Array

dataFile = "11_data.txt"

calcGrid n g = maximum . concat $ [[hs,vs,das,dbs] | let is = range . bounds $ g,
                                                let inBounds x = x `elem` is,
                                                let prod f ix = product . map (g!) . filter inBounds . take n . f $ ix,
                                                i <- is,
                                                let hs = prod horz i,
                                                let vs = prod vert i,
                                                let das = prod diagA i,
                                                let dbs = prod diagB i]
    where
        horz  i@(x,y) = i : (map (\n -> (x+n,y)) $ [1..])
        vert  i@(x,y) = i : (map (\n -> (x,y+n)) $ [1..]) 
        diagA i@(x,y) = i : (map (\n -> (x+n,y+n)) $ [1..])
        diagB i@(x,y) = i : (map (\n -> (x+n,y-n)) $ [1..])

io f = do
    s <- readFile dataFile
    let grid = listArray ((1,1),(20,20)) . map (read :: String -> Int) . words $ s
    putStr . show . f $ grid

main = io . calcGrid $ 4
