-- project euler problem 6

solve numbers = squareSum numbers - sumSquares numbers 
    where
        sumSquares numbers = sum $ map (^2) numbers
        squareSum numbers = (sum numbers)^2

main = print $ solve [1..100]
