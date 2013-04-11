module Problem66 where

import qualified Problem64

--solve = aa 3
solve = maximum . solutions 
solutions d = map (\a -> (solutionPairs a, a)) $ Problem64.nonSquaresUnder d
solutionPairs = foldr (\x (a,b) -> (a*x+b, a)) (1,0) . fractions
solutionScan = scanr (\x (a,b) -> (a*x+b, a)) (1,0) . fractions
fractions d = map (fst) list
        where list 
                | length equations == 2 = equations
                | otherwise = init equations
              equations = diophantineEquation d
diophantineEquation d = Problem64.sqrtAsContinuedFractions d

