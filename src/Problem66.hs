module Problem66 where

import qualified Problem64

solve :: Integer -> Integer
solve = snd . maximum . solutions
 
solutions :: Integer -> [((Integer, Integer), Integer)]
solutions d = map (\a -> (minPairs a, a)) $ Problem64.nonSquaresUnder d

minPairs :: Integer -> (Integer, Integer)
minPairs d = head $ filter (\(x,y) -> equationMatches x y d) $ continuedFractionPairs d

equationMatches :: (Eq a, Num a) => a -> a -> a -> Bool
equationMatches x y d =  x*x-d*y*y == 1

continuedFractionPairs :: Integer -> [(Integer, Integer)]
continuedFractionPairs = map calcPair . fractions

calcPair :: [Integer] -> (Integer, Integer)
calcPair = foldr (\x (a,b) -> (a*x+b, a)) (0,1)

fractions :: Integral a => a -> [[a]]
fractions d = [nthFraction d a | a<-[0..]]

nthFraction :: Integral a => a -> Int -> [a]
nthFraction d n = firstFraction ++ take fractionLength (cycle repeats)
        where fractionLength = (length repeats)*n
              repeats = tail firstFraction
              firstFraction = fraction d

fraction :: Integral b => b -> [b]
fraction = map (fst) . Problem64.sqrtAsContinuedFractions