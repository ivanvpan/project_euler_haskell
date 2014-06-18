largestPali :: Int
largestPali = maximum [(a * b) | a <- [100..999], b <- [100..999], (isPali . show) (a * b)]
    where
        isPali :: String -> Bool
        isPali text = text == reverse text

main = print largestPali
