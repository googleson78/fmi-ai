module Main where

import Xno
import qualified Data.Vector as Vec

main :: IO ()
main = print . play X . start . read =<< getLine

start :: Int -> GameState
start n = GameState
  { currPlayer = X
  , currBoard = Board $ Vec.replicate n $ Vec.replicate n Nothing
  }
