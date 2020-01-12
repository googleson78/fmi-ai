{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module MinConflicts where

import Control.Monad (replicateM)
import Data.List (intercalate, delete)
import Data.List.Extra (minimumOn)
import Prelude hiding (curry)
import Data.Vector (Vector, (!))
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
  => m (Vector Location)
conflicting = gets \BoardState{board, conflicts} ->
  flip Vec.imapMaybe board \x y ->
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

-- TODO: why are the diag filters necessary
-- I thought I removed them in the diag generation..
updateStraightConflicts :: Int -> Location -> Int -> [([Location], Int -> Int)]
updateStraightConflicts n (x, oldy) newy =
  let oldCol = filter (\loc -> not $ diag loc (x, newy)) $ genCol x n oldy
      newCol = filter (\loc -> not $ diag loc (x, oldy)) $ genCol x n newy
   in [(newCol, (+1)), (oldCol, (+ (-1)))]

genRow :: Int -> Int -> a -> [(a, Int)]
genRow col n x = map (x,) $ fromToExcept 0 n col
{-# INLINE genRow #-}

genCol :: Int -> Int -> a -> [(Int, a)]
genCol row n y = map (,y) $ fromToExcept 0 n row
{-# INLINE genCol #-}

diag :: Location -> Location -> Bool
diag (x, y) (x', y')
  = (x, y) /= (x', y')
 && (abs (x - x') == abs (y - y'))
{-# INLINE diag #-}

revDiag :: (Int, Int) -> Int -> [(Int, Int)]
revDiag (x', y') n = [(z, m - z) | m <- [x' + y'], z <- [0..m], z <= n, m - z <= n, z /= x']

revDiag' :: (Int, Int) -> Int -> Int -> [(Int, Int)]
revDiag' (x', y') n excy = [(z, m - z) | m <- [x' + y'], z <- [0..m], z <= n, m - z <= n, z /= x', (m - z) /= excy]

-- TODO: move the i /= * stuff to delete, probably
mainDiag :: (Int, Int) -> Int -> [(Int, Int)]
mainDiag (x', y') n =
  let d = abs (x' - y')
   in case compare x' y' of
        EQ -> [(i, i) | i <- [0..n], i /= x']
        LT -> [(i, i + d) | i <- [0..n - d], i /= x']
        GT -> [(i + d, i) | i <- [0..n - d], i /= y']

-- TODO: move the excy stuff to delete, probably
-- excy is used to exclude crossing points with columns
mainDiag' :: (Int, Int) -> Int -> Int -> [(Int, Int)]
mainDiag' (x', y') n excy =
  let d = abs (x' - y')
   in case compare x' y' of
        EQ -> [(i, i) | i <- [0..n], i /= x', i /= excy]
        LT -> [(i, i + d) | i <- [0..n - d], i /= x', (i + d) /= excy]
        GT -> [(i + d, i) | i <- [0..n - d], i /= y', i /= excy]

updateDiagConflicts :: Int -> Location -> Int -> [([Location], Int -> Int)]
updateDiagConflicts n (x, oldy) newy =
  let oldRevDiag = revDiag' (x, oldy) n newy
      newRevDiag = revDiag' (x, newy) n oldy

      oldMainDiag = mainDiag' (x, oldy) n newy
      newMainDiag = mainDiag' (x, newy) n oldy

      oldDiag = oldRevDiag ++ oldMainDiag
      newDiag = newRevDiag ++ newMainDiag

      delCrossing xs =
        let dy = abs $ oldy - newy
            dist = div dy 2
            st = min oldy newy
         in
          if even dy
          then delete (x - dist, st + dist) $ delete (x + dist, st + dist) xs
          else xs

      oldDiag' = delCrossing oldDiag

      newDiag' = delCrossing newDiag

   in [(newDiag', (+1)), (oldDiag', (+ (-1)))]

randomConflicting
  :: MonadState BoardState m
  => m Location
randomConflicting = do
  BoardState {gen} <- get
  conflicts <- conflicting

  let n = length conflicts
      (ind, gen') = randomR (0, n - 1) gen


  modify' \old -> old {gen = gen'}

  pure $ conflicts ! ind

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

fillRandom :: Int -> IO BoardState
fillRandom n = do
  randomPlaces <- replicateM n $ randomRIO (0, n - 1)
  gen <- newStdGen
  let board = Vec.fromList randomPlaces
      emptyConflicts = mkConflicts n
      conflicts
        = foldr (adjust succ) emptyConflicts $ Vec.indexed board

  pure $ BoardState{..}

placeConflicts :: Int -> Location -> [([Location], Int)]
placeConflicts n loc = placeStraightConflicts n loc ++ placeDiagConflicts n loc

placeStraightConflicts :: Int -> Location -> [([Location], Int)]
placeStraightConflicts n (x, y) = [(genRow y n x, 1), (genCol x n y, 1)]

placeDiagConflicts :: Int -> Location -> [([Location], Int)]
placeDiagConflicts n loc = [(mainDiag loc n, 1), (revDiag loc n, 1)]

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
  (success, result) <- runState (minimiseConflicts $ 2 * n) <$> fillRandom n
  if success
  then pure result
  else solveFor n
