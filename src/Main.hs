module Main where

import qualified Problem64
import Problem66

main::IO()
main = do
        putStrLn "Project euler with haskell"
        print Problem64.solve
        print $ Problem66.diophantineEquation 2
