-- euler 3

primes :: Integral a => [a]
primes = primes' [] 2
    where
        primes' :: Integral a => [a] -> a -> [a]
        primes' previous current
            | isPrime current previous = current : primes' (previous ++ [current]) (succ current)
            | otherwise = primes' (previous ++ [current]) (succ current)

isPrime :: Integral a => a -> [a] -> Bool
isPrime a [] = True
isPrime a (x:xs)  
    | a `mod` x == 0 = False
    | x > floor (sqrt (fromIntegral a)) = True
    | otherwise = isPrime a xs

primeDivisors :: Integral a => a -> [a]
primeDivisors n = primeDivisors' n [] primes
    where
        primeDivisors' :: Integral a => a -> [a] -> [a] -> [a]
        primeDivisors' n divisors (x:xs)
            | x > floor (sqrt (fromIntegral n)) = divisors
            | n `mod` x == 0 = primeDivisors' n (x:divisors) xs
            | otherwise = primeDivisors' n divisors xs

main = print $ head $ primeDivisors 600851475143
