module Main (main) where

import MinConflicts (solveForRetry, prettyState)
import Data.Foldable (traverse_)

main :: IO ()
main = traverse_ prettyState =<< solveForRetry 10 =<< readLn
