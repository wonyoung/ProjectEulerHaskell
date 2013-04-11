module Problem64 where

import Data.List

solve = countOddUnder 10000
countOddUnder n = length $ filter (odd) $ periods n
periods n = map period $ nonSquaresUnder n
period = length . tail . map fst .sqrtAsContinuedFractions

sqrtAsContinuedFractions :: Integral t =>
                                       t -> [(t, (t, t, t))]
sqrtAsContinuedFractions n = reverse $ continuedFractions firstFraction []
        where firstFraction = (integerPart, (1, n, integerPart))
              integerPart = floorSqrt n

continuedFractions :: Integral t =>
        (t, (t, t, t)) -> [(t, (t, t, t))] -> [(t, (t, t, t))]
continuedFractions (i, a) list 
        | (i, a) `elem` list = list
        | otherwise = continuedFractions nf ((i,a):list)  
        where nf = nextFraction a

nextFraction :: (Integral t) =>
        (t, t, t) -> (t, (t, t, t))
nextFraction (de, n, k) = ( integerPart, fractionPart )
        where fractionPart = (nextDe, n, nextK)
              nextK = integerPart*nextDe - k
              integerPart = div (floorSqrt n + k) nextDe
              nextDe = div (n - k*k) de

-- To Do:: implement without floor, sqrt
floorSqrt :: (Integral a) => a -> a
floorSqrt = floor . sqrt . fromIntegral
--floorSqrt n = head [i | i<-reverse [1..n], i*i < n]

nonSquaresUnder n = [1..n] \\ squaresUnder n
squaresUnder n = [i*i| i<-[1..n]]