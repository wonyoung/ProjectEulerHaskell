module Problem22 where

import Data.Char
import Data.List

solve :: IO ()
solve = do
        contents <- readFile "names.txt"
        print ((sum . nameScore . map toScore . sort . names) contents) 

nameScore :: [Int] -> [Int]
nameScore = zipWith (*) [1..]

toScore :: String -> Int
toScore sx = sum $ map charScore sx

charScore :: Char -> Int
charScore c = ord c - ord 'A' + 1

names :: String -> [String]
names s = read ("[" ++ s ++ "]")::[String]
--names = map (filter isAlpha) . words . replaceCommaToSpace

--replaceCommaToSpace :: String -> String
--replaceCommaToSpace = map (\c -> if c==',' then ' ' else c)
