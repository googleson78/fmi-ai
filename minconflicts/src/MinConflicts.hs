{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module MinConflicts
  ( solveFor
  , prettyState
  ) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.List.Extra (minimumOn)
import Prelude hiding (curry)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as Mut (write)
import Conflicts (Conflicts, mkConflicts, isConflicting, adjust, ix)
import Control.Monad.State.Class (MonadState(..), gets, modify')
import Control.Monad.State.Strict (runState)
import System.Random (StdGen, randomR, newStdGen)


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
  anyConflicting >>= \case
    True -> do

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
      pure True
    False -> pure False

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
  startgen <- newStdGen

  let go currb currconf currGen cx
        | cx == n = (currconf, currb, currGen)
        | otherwise =
            let !potential = map (\y' -> (y', ix (cx, y') currconf)) [0..n - 1]
                (_, !smallest) = minimumOn snd potential
                !potential' = filter ((==smallest) . snd) potential
                (!index, !nextGen) = randomR (0, length potential' - 1) currGen
                (!newy, _) = potential' !! index
                !nextConflicts = adjust succ (cx, newy) currconf
                !nextBoard = Vec.modify (\currb' -> Mut.write currb' cx newy) currb
             in go nextBoard nextConflicts nextGen (cx + 1)

      (!conflicts, !board, !gen) = go (Vec.replicate n 0) emptyConflicts startgen (0 :: Int)

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
  (success, result) <- runState (minimiseConflicts $ 3 * n) <$> fillRandom empty n
  if success
  then pure result
  else solveFor n
