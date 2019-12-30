module Main (main) where

import Bfs (Board(..), findPath)
import Control.Monad (replicateM)
import qualified Data.Vector as Vec
import Data.Foldable (traverse_)

main :: IO ()
main = do
  let readInput :: (Read a) => IO a
      readInput = readLn
  n <- readInput
  rows <- replicateM n readInput
  (start, end) <- readInput
  let board = Board $ Vec.fromList rows

  traverse_ print $ findPath board start end
