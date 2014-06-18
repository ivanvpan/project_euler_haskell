factors :: Integral a => a -> [a]
factors 0 = []
factors 1 = []
factors n = f: factors (n `div` f)
    where f = head $ filter (\d -> n `mod` d == 0) [2..n]
