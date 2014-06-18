import qualified Data.Map as Map
import Data.Maybe (fromJust)

factors :: Integral a => a -> Map.Map a [a]
factors 0 = Map.empty
factors 1 = Map.empty
factors n = 
    let results = factors (n `div` f)
    in
        case Map.lookup f results of
            Nothing -> Map.insert f [f] results
            Just xs -> Map.insert f (f:xs) results
    where f = head $ filter (\d -> n `mod` d == 0) [2..n]

commonFactors numbers = foldl pickFactors Map.empty (allFactors numbers)
    where
        allFactors :: Integral a => [a] -> [(a, [a])]
        allFactors numbers = (foldl (++) [] (map (Map.toList . factors) numbers))
        pickFactors :: Integral a => Map.Map a [a] -> (a, [a]) -> Map.Map a [a]
        pickFactors acc current =
            case Map.lookup (fst current) acc of
                Just xs ->
                    if length (snd current) > length xs
                        then Map.insert (fst current) (snd current) acc
                        else acc
                Nothing -> Map.insert (fst current) (snd current) acc

smallestDivisible :: Integral a => [a] -> a
smallestDivisible numbers = foldl (*) 1 concat
    where
        concat = foldl (\acc x -> (snd x) ++ acc) [] (Map.toList $ commonFactors numbers)

main = print $ smallestDivisible [1..20]
