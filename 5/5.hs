import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- 1. factors as map fac -> [fac, fac, fac...]
-- 2. combine factors from all number into an array [(fac, [fac, fac, fac...])]
-- 3. keep factors maps with high occurence by pushing back into map
-- Damn it's hard to read. *scratchhead*

factorFrequency :: Integral a => a -> Map.Map a [a]
factorFrequency 0 = Map.empty
factorFrequency 1 = Map.empty
factorFrequency n = 
    let results = factorFrequency (n `div` f)
    in
        case Map.lookup f results of
            Nothing -> Map.insert f [f] results
            Just xs -> Map.insert f (f:xs) results
    where f = head $ filter (\d -> n `mod` d == 0) [2..n]

commonFactors :: Integral a => [a] -> Map.Map a [a]
commonFactors numbers = foldl pickFactors Map.empty (allFactors numbers)
    where
        allFactors :: Integral a => [a] -> [(a, [a])]
        allFactors numbers = (foldl (++) [] (map (Map.toList . factorFrequency) numbers))

        pickFactors :: Integral a => Map.Map a [a] -> (a, [a]) -> Map.Map a [a]
        pickFactors acc current = Map.insertWith compare (fst current) (snd current) acc
        compare new old =
            if length new > length old
                then new 
                else old

smallestDivisible :: Integral a => [a] -> a
smallestDivisible numbers = foldl (*) 1 concatFactors
    where
        concatFactors = foldl (\acc x -> (snd x) ++ acc) [] (Map.toList $ commonFactors numbers)

main = print $ smallestDivisible [1..20]
