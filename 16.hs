sumDigits n = sum . map (read . return) . show $ 2 ^ n

main = print $ sumDigits 1000 
