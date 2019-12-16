module Main where

import Xno
import qualified Data.Vector as Vec

main :: IO ()
main = print $ play X start

start :: GameState
start = GameState
  { currPlayer = X
  , currBoard = Board $ Vec.replicate 3 $ Vec.replicate 3 Nothing
  }
