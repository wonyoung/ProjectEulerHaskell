module Example.Calc where

import Data.Char

isDigitStr:: String -> Bool
--isDigitStr list = foldl (&&) True $ map isDigit list
--isDigitStr = foldl (\x y -> x && (isDigit y)) True
isDigitStr [] = True
isDigitStr (a:ax)
        | isDigit a == False = False
        | otherwise = isDigitStr ax


solve::String -> Double
solve = head . calc [] . words

calc stack [] = stack
calc stack (a:ax) = calc (push a stack) ax

push "+" (x:y:rest) = (y+x):rest
push "-" (x:y:rest) = (y-x):rest
push "*" (x:y:rest) = (y*x):rest
push e rest = (read e):rest
