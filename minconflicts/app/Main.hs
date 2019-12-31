module Main (main) where

import MinConflicts (solveFor, prettyState)

main :: IO ()
main = prettyState =<< solveFor =<< readLn
