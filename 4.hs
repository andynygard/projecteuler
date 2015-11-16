import Control.Applicative

isPalindrome a = show a == reverse (show a)

main = print $ maximum . filter isPalindrome $ values
    where
        values = (*) <$> [100..999] <*> [100..999]
