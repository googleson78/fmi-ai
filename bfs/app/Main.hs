module Main (main) where

import Bfs (Board(..), BfsState(..), bfs)
import Control.Monad (replicateM)
import qualified Data.Vector as Vec
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (execState)
import qualified Data.HashSet as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Hashable (Hashable)

main :: IO ()
main = do
  let readInput :: (Read a) => IO a
      readInput = readLn
  n <- readInput
  rows <- replicateM n readInput
  (start, end) <- readInput
  let board = Board $ Vec.fromList rows
      startState = BfsState Set.empty Seq.empty Map.empty
  print $ reverse $ getParents end $ paths $ (`execState` startState) $ (`runReaderT` board) $ bfs start

getParents :: (Eq a, Hashable a) => a -> HashMap a a -> [a]
getParents x m = x : case Map.lookup x m of
  Nothing -> []
  Just y -> getParents y m
