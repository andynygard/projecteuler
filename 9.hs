fa b l = (0.5 * l^2 - l * b) / (l - b)

isInt x = x == fromIntegral (round x)

triple l = [[a,b,c] | b <- [2..499],
                      let a = fa b l,
                      let c = sqrt(a^2 + b^2),
                      a < b,
                      isInt a,
                      isInt c]

main = print . round . product . head . triple $ 1000
