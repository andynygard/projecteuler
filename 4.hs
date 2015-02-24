isPalindrome a = show a == reverse (show a)

main = print $ maximum . filter isPalindrome $ values
    where
        values = do
            x <- [100..999]
            y <- [100..999]
            return (x * y)
