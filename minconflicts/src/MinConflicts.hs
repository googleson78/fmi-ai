{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module MinConflicts
  ( solveFor
  , prettyState
  ) where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.List.Extra (minimumOn)
import Prelude hiding (curry)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as Mut (write)
import Conflicts (Conflicts, mkConflicts, isConflicting, adjust, ix)
import Control.Monad.State.Class (MonadState(..), gets, modify')
import Control.Monad.State.Strict (runState)
import System.Random (StdGen, randomR, randomRIO, newStdGen)


type Board = Vector Int
type Location = (Int, Int)

data BoardState = BoardState
  { board :: !Board
  , conflicts :: !Conflicts
  , gen :: !StdGen
  }
  deriving Show

conflicting
  :: MonadState BoardState m
  => m [Location]
conflicting = gets \BoardState{board, conflicts} ->
  flip mapMaybe (Vec.toList $ Vec.indexed board) \(x, y) ->
    if isConflicting (x, y) conflicts
    then Just (x, y)
    else Nothing

anyConflicting
  :: MonadState BoardState m
  => m Bool
anyConflicting = gets \BoardState{board, conflicts} ->
  Vec.any (`isConflicting` conflicts) $ Vec.indexed board

-- assume we only move things around in the same row!!
move
  :: Location -> Location -> Board -> Board
move (oldx, _) (_, newy) board =
  Vec.modify (\mboard -> Mut.write mboard oldx newy) board

-- assume we only move around the same row,
-- so we don't have any conflicts in rows!
updateConflicts :: Location -> Location -> Conflicts -> Conflicts
updateConflicts oldloc newloc
  = adjust (+ (-1)) oldloc
  . adjust (+1) newloc

randomConflicting
  :: MonadState BoardState m
  => m Location
randomConflicting = do
  BoardState {gen} <- get
  conflicts <- conflicting

  let n = length conflicts
      (ind, gen') = randomR (0, n - 1) gen


  modify' \old -> old {gen = gen'}

  pure $ conflicts !! ind

minimiseConflicts
  :: MonadState BoardState m
  => Int -> m Bool
minimiseConflicts = flip boundedWhileM do
  BoardState {board, conflicts} <- get

  (cx, cy) <- randomConflicting

  let newy
        = minimumOn (\y' -> ix (cx, y') conflicts)
        $ fromToExcept 0 (length board - 1) cy
      newConflicts = updateConflicts (cx, cy) (cx, newy) conflicts
      newBoard = move (cx, cy) (cx, newy) board

  modify' \old ->
    old
      { conflicts = newConflicts
      , board = newBoard
      }

  anyConflicting

fromToExcept :: Int -> Int -> Int -> [Int]
fromToExcept start end except = filter (/=except) [start..end]
{-# INLINE fromToExcept #-}

boundedWhileM :: Monad m => Int -> m Bool -> m Bool
boundedWhileM maxSteps act = go 0
  where
    go i
      | i == maxSteps = pure False
      | otherwise = do
        x <- act
        if x
        then go (i + 1)
        else pure True

fillRandom :: Conflicts -> Int -> IO BoardState
fillRandom emptyConflicts n = do
  randomPlaces <- replicateM n $ randomRIO (0, n - 1)
  gen <- newStdGen
  let board = Vec.fromList randomPlaces
      conflicts
        = foldl' (flip $ adjust succ) emptyConflicts $ zip [0..] randomPlaces

  pure $ BoardState{..}

printAsBoard :: Board -> String
printAsBoard v = concat $ flip Vec.imap v \_ y -> rowWithMarked (length v - 1) y ++ "\n"

prettyState :: BoardState -> IO ()
prettyState BoardState{board} = do
  putStrLn "Board:"
  putStr $ printAsBoard board
  putStrLn ""

rowWithMarked :: Int -> Int -> String
rowWithMarked len marked = intercalate "|" $ flip map [0..len] \n ->
  if n == marked
  then "Q"
  else "_"

solveFor :: Int -> IO BoardState
solveFor n = do
  let empty = mkConflicts n
  (success, result) <- runState (minimiseConflicts $ 2 * n) <$> fillRandom empty n
  if success
  then pure result
  else solveFor n
