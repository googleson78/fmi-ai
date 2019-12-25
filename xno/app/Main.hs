module Main where

import Xno
import qualified Data.Vector as Vec

main :: IO ()
main = print . play X . start . read =<< getLine

start :: Int -> Board a
start n = Board $ Vec.replicate n $ Vec.replicate n Nothing
