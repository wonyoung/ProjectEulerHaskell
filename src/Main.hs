module Main where

import qualified Problem22
import qualified Problem64
import qualified Problem66

main::IO()
main = do
        putStrLn "Project euler with haskell"
--        Problem22.solve
--        print Problem64.solve
        print $ Problem66.solve 1000

        
        module Main where

import qualified Calc
import qualified LGCodeJam2013

import Data.Ix
import Data.List
import Data.Tuple
import qualified Numeric


main::IO()
main = do
--        print $ Calc.solve "10 4 3 + 2 * -"
--        print $ LGCodeJam2013.solve1
        contents <- readFile "input.txt"
--        putStrLn $ contents
--        putStrLn $ showUsageWhen 11 $ parsePrograms contents
        putStr $ solve contents
--        print $ map (\x -> programsUsed x programs inputs) programs


runningPrograms :: (Integer) -> [(String, Integer, Integer, Integer)] -> [(String, Float)]
runningPrograms when programList = (map (\(n,s,e,u) -> (n,fromInteger u / fromInteger (e - s))) . 
        filter (\(n,s,e,u) -> inRange (s,e-1) when)) programList

calcUsage :: [(String, Float)] -> [(String, Float)]
calcUsage programs = map (\(name, usage) -> (name, usage / totalUsages)) programs
        where totalUsages = sum (map snd programs)

sortUsage :: [(String, Float)] -> [(String, Float)]
sortUsage = sortBy singleComparer

singleComparer :: Ord a =>
                             (a, t) -> (a, t1) -> Ordering
singleComparer (a, b) (c, d) = compare a c


showUsage :: [(String, Float)] -> [String]
showUsage = map (\(name, usage) -> name ++ " " ++ showFloat usage)

showFloat :: Float -> String
showFloat = flip (++) "%" . flip (Numeric.showFFloat (Just 1)) "" . (*) 100

showUsageWhen :: (Integer) -> [(String, Integer, Integer, Integer)] -> String
showUsageWhen when = unlines . showUsage . sortUsage . calcUsage . runningPrograms when 

solve :: String -> String
solve input = showUsages when programs
        where when = parseInput input
              programs = parsePrograms input

showUsages :: [Integer] -> [(String, Integer, Integer, Integer)] -> String
showUsages a b = foldl (\acc x -> acc ++ (show x) ++ "\n"++ (showUsageWhen x b)++ "\n") "" a
--programsUsed x programs inputs = filter (isUsed x) programs inputs
        
parseInput :: String -> [Integer]
parseInput = (map read) . words . head . lines

parsePrograms :: String -> [(String, Integer, Integer, Integer)]
parsePrograms = (map parseProgram) . tail . lines

parseProgram :: String -> (String, Integer, Integer, Integer)
parseProgram s = (name, read start, read end, read usage)
        where [name, start, end, usage] = words s
        
        
        