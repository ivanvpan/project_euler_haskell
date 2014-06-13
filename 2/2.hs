-- euler 2

{-
fib :: Integral a => a -> a
fib 0 = 1
fib 1 = 1
fib n =  fib (n - 1) + fib (n - 2)

fib :: Integral a => a -> a -> a -> a
fib a b limit =
    if a > limit
        then 0
        else
            if even a
                then a + (fib b (a + b) limit)
                else fib b (a + b) limit
main = print $ fib 1 2 4000000
-}

--fib' :: Integral a => a -> a -> [a]
--fib' 

fib :: Integral a => [a]
fib = fib' 1 2
    where fib' a b = a : fib' b (a + b)

fibEvenSum limit = sum $ filter even (takeWhile (< limit) fib)

main = print $ fibEvenSum 4000000
