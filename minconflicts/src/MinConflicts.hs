{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MinConflicts where

import Control.Monad (replicateM)
import Data.List (sortOn, intercalate)
import Data.List.Extra (minimumOn, groupOn)
import Prelude hiding (curry)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as Mut (write)
import TupMap (TupMap)
import qualified TupMap as Map
import Control.Monad.State.Class (MonadState(..), gets, modify')
import Control.Monad.State.Strict (runState)
import System.Random (StdGen, randomR, randomRIO, newStdGen)


type Board = Vector Int
type Location = (Int, Int)
type Conflicts = TupMap Int

data BoardState = BoardState
  { board :: !Board
  , conflicts :: !Conflicts
    -- ^ consider using an unboxed vector of triples (x, y, conflicts) instead
    -- the hashmap isn't useful at all
  , gen :: !StdGen
  }
  deriving Show

conflicting
  :: MonadState BoardState m
  => m (Vector Location)
conflicting = gets \BoardState{board, conflicts} ->
  flip Vec.imapMaybe board \x y ->
    if Map.ix (x, y) conflicts == 0
    then Nothing
    else Just (x, y)

-- assume we only move things around in the same row!!
move
  :: Location -> Location -> Board -> Board
move (oldx, _) (_, newy) board =
  Vec.modify (\mboard -> Mut.write mboard oldx newy) board

-- assume we only move around the same row,
-- so we don't have any conflicts in rows!
updateConflicts :: Int -> Location -> Location -> Conflicts -> Conflicts
updateConflicts n oldloc newloc@(_, newy) =
  let updates
        = [([oldloc], (+1))]
       ++ [([newloc], (+ (-1)))]
       ++ updateStraightConflicts n oldloc newy
       ++ updateDiagConflicts n oldloc newy
   in Map.adjustBulk updates

updateStraightConflicts :: Int -> Location -> Int -> [([Location], Int -> Int)]
updateStraightConflicts n (x, oldy) newy =
  let genCol y' = [(x', y') | x' <- fromToExcept 0 n x]
      {-# INLINE genCol #-}
      oldCol = genCol oldy
      {-# INLINE oldCol #-}
      newCol = genCol newy
      {-# INLINE newCol #-}
   in [(newCol, (+1)), (oldCol, (+ (-1)))]
{-# INLINE updateStraightConflicts #-}

diag :: Location -> Location -> Bool
diag (x, y) (x', y')
  = (x, y) /= (x', y')
 && (abs (x - x') == abs (y - y'))
{-# INLINE diag #-}

revDiag :: (Int, Int) -> Int -> [(Int, Int)]
revDiag (x', y') n = [(z, m - z) | m <- [x' + y'], z <- [0..m], z <= n, m - z <= n, z /= x']
{-# INLINE revDiag #-}

mainDiag :: (Int, Int) -> Int -> [(Int, Int)]
mainDiag (x', y') n =
  let d = abs (x' - y')
   in case compare x' y' of
        EQ -> [(i, i) | i <- [0..n], i /= x']
        LT -> [(i, i + d) | i <- [0..n - d], i /= x']
        GT -> [(i + d, i) | i <- [0..n - d], i /= y']
{-# INLINE mainDiag #-}

updateDiagConflicts :: Int -> Location -> Int -> [([Location], Int -> Int)]
updateDiagConflicts n (x, oldy) newy =
  let oldRevDiag = revDiag (x, oldy) n
      {-# INLINE oldRevDiag #-}
      newRevDiag = revDiag (x, newy) n
      {-# INLINE newRevDiag #-}

      oldMainDiag = mainDiag (x, oldy) n
      {-# INLINE oldMainDiag #-}
      newMainDiag = mainDiag (x, newy) n
      {-# INLINE newMainDiag #-}

      oldDiag = oldRevDiag ++ oldMainDiag
      {-# INLINE oldDiag #-}
      newDiag = newRevDiag ++ newMainDiag
      {-# INLINE newDiag #-}

   in [(newDiag, (+1)), (oldDiag, (+ (-1)))]
{-# INLINE updateDiagConflicts #-}

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
  => Int -> Int -> m Bool
minimiseConflicts n = flip boundedWhileM do
  BoardState {board, conflicts} <- get

  (cx, cy) <- randomConflicting

  let newy
        = minimumOn (\y' -> conflicts Map.! (cx, y'))
        $ fromToExcept 0 (length board - 1) cy
  modify' \old ->
    old
      { conflicts = updateConflicts n (cx, cy) (cx, newy) conflicts
      , board = move (cx, cy) (cx, newy) board
      }
  pure $ conflicts Map.! (cx, newy) /= 0

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
      emptyConflicts = Map.mkTupMap n n 0
      conflicts = Vec.ifoldl' (\acc x y -> placeConflicts acc (x, y)) emptyConflicts board

  pure $ BoardState{..}

placeConflicts :: Conflicts -> Location -> Conflicts
placeConflicts cs loc
  = placeStraightConflicts loc
  $ placeDiagConflicts loc
  $ cs

placeStraightConflicts :: Location -> Conflicts -> Conflicts
placeStraightConflicts (x, y) cs =
  flip Map.mapWithKey cs \(currx, curry) c ->
    if currx == x
    then
      if curry == y
      then c
      else c + 1
    else
      if curry == y
      then c + 1
      else c

placeDiagConflicts :: Location -> Conflicts -> Conflicts
placeDiagConflicts loc cs =
  flip Map.mapWithKey cs \currloc c ->
  if currloc == loc
  then c
  else
    if diag currloc loc
    then c + 1
    else c

printAsBoard :: Board -> String
printAsBoard v = concat $ flip Vec.imap v \_ y -> rowWithMarked (length v - 1) y ++ "\n"

printConflicts :: Conflicts -> String
printConflicts (Map.toList -> sortOn fst -> groupOn (fst . fst) -> css)
  = concatMap (\cs -> '\n' : concatMap (show . snd) cs) css

prettyState :: BoardState -> IO ()
prettyState BoardState{board, conflicts} = do
  putStrLn "Board:"
  putStr $ printAsBoard board
  putStr "Conflicts:"
  putStr $ printConflicts conflicts
  putStrLn ""

rowWithMarked :: Int -> Int -> String
rowWithMarked len marked = intercalate "|" $ flip map [0..len] \n ->
  if n == marked
  then "Q"
  else "_"

solveFor :: Int -> IO BoardState
solveFor n = do
  (success, result) <- runState (minimiseConflicts (n - 1) $ 2 * n) <$> fillRandom n
  if success
  then pure result
  else solveFor n
