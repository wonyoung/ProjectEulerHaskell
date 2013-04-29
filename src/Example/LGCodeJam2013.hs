module Example.LGCodeJam2013 where

import Data.Ix
import Data.List
import Data.Tuple
import qualified Numeric

main::IO()
main = do
        contents <- readFile "input.txt"
        putStr $ solve contents

solve :: String -> String
solve input = showUsages when programs
        where when = parseInput input
              programs = parsePrograms input

showUsages :: [Integer] -> [(String, Integer, Integer, Integer)] -> String
showUsages a b = foldl (\acc x -> acc ++ (show x) ++ "\n"++ (showUsageWhen x b)++ "\n") "" a

showUsageWhen :: (Integer) -> [(String, Integer, Integer, Integer)] -> String
showUsageWhen when = unlines . showUsage . sortUsage . calcUsage . runningPrograms when 

showUsage :: [(String, Float)] -> [String]
showUsage = map (\(name, usage) -> name ++ " " ++ showFloat usage)

showFloat :: Float -> String
showFloat = flip (++) "%" . flip (Numeric.showFFloat (Just 1)) "" . (*) 100

runningPrograms :: (Integer) -> [(String, Integer, Integer, Integer)] -> [(String, Float)]
runningPrograms when = (map (\(n,s,e,u) -> (n,fromInteger u / fromInteger (e - s))) . 
        filter (\(_,s,e,_) -> inRange (s,e-1) when))

calcUsage :: [(String, Float)] -> [(String, Float)]
calcUsage programs = map (\(name, usage) -> (name, usage / totalUsages)) programs
        where totalUsages = sum (map snd programs)

sortUsage :: [(String, Float)] -> [(String, Float)]
sortUsage = map swap . reverse . sortBy singleComparer . map swap . sortBy singleComparer

singleComparer :: Ord a => (a, t) -> (a, t1) -> Ordering
singleComparer (a, _) (c, _) = compare a c

parseInput :: String -> [Integer]
parseInput = (map read) . words . head . lines

parsePrograms :: String -> [(String, Integer, Integer, Integer)]
parsePrograms = (map parseProgram) . tail . lines

parseProgram :: String -> (String, Integer, Integer, Integer)
parseProgram s = (name, read start, read end, read usage)
        where [name, start, end, usage] = words s