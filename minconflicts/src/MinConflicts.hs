{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module MinConflicts where

import Control.Monad (replicateM)
import Data.List (sortOn, intercalate)
import Data.List.Extra (minimumOn, groupOn)
import Prelude hiding (curry)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as Mut (write)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Control.Monad.State.Class (MonadState(..), gets, modify')
import Control.Monad.State.Strict (runState)
import System.Random (StdGen, randomR, randomRIO, newStdGen)


type Board = Vector Int
type Location = (Int, Int)
type Conflicts = HashMap Location Int

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
    case Map.lookup (x, y) conflicts of
      Nothing -> error "shouldn't happen"
      Just n
        | n == 0 -> Nothing
        | otherwise -> Just (x, y)

-- assume we only move things around in the same row!!
move
  :: Location -> Location -> Board -> Board
move (oldx, _) (_, newy) board =
  Vec.modify (\mboard -> Mut.write mboard oldx newy) board

-- assume we only move around the same row,
-- so we don't have any conflicts in rows!
updateConflicts :: Location -> Location -> Conflicts -> Conflicts
updateConflicts oldloc newloc@(_, newy) conflicts
  = updateSpots oldloc newloc
  $ updateStraightConflicts oldloc newy
  $ updateDiagConflicts oldloc newy
  $ conflicts

updateSpots :: Location -> Location -> Conflicts -> Conflicts
updateSpots oldloc newloc =
  Map.mapWithKey \currloc cs ->
    if
      | currloc == oldloc -> cs + 1
      | currloc == newloc -> cs - 1
      | otherwise -> cs
{-# INLINE updateSpots #-}

updateStraightConflicts :: Location -> Int -> Conflicts -> Conflicts
updateStraightConflicts (x, oldy) newy =
  Map.mapWithKey \(currx, curry) cs ->
    if x == currx
    then cs
    else
      if
        | curry == oldy -> cs - 1
        | curry == newy -> cs + 1
        | otherwise -> cs
{-# INLINE updateStraightConflicts #-}

diag :: Location -> Location -> Bool
diag (x, y) (x', y')
  = (x, y) /= (x', y')
 && (abs (x - x') == abs (y - y'))
{-# INLINE diag #-}

updateDiagConflicts :: Location -> Int -> Conflicts -> Conflicts
updateDiagConflicts (x, oldy) newy =
  Map.mapWithKey \(currx, curry) cs ->
    if
      | diag (x, oldy) (currx, curry) && diag (x, newy) (currx, curry) -> cs
      | diag (x, oldy) (currx, curry) -> cs - 1
      | diag (x, newy) (currx, curry) -> cs + 1
      | otherwise -> cs
{-# INLINE updateDiagConflicts #-}

countConflicts :: Conflicts -> Board -> Int
countConflicts cs = Vec.ifoldl (\acc x y -> acc + cs Map.! (x, y)) 0

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

  let (minConflicts, minBoard)
        = minimumOn (uncurry countConflicts)
        $ map (\y' -> (updateConflicts (cx, cy) (cx, y') conflicts, move (cx, cy) (cx, y') board))
        $ fromToExcept 0 (length board - 1) cy
  modify' \old ->
    old
      { conflicts = minConflicts
      , board = minBoard
      }
  pure $ countConflicts minConflicts minBoard /= 0

fromToExcept :: Int -> Int -> Int -> [Int]
fromToExcept start end except = filter (/=except) [start..end]

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
      emptyConflicts = Map.fromList $ flip zip (repeat 0) [(x, y) | x <- [0..n - 1], y <- [0..n - 1]]
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
  (success, result) <- runState (minimiseConflicts $ 10 * n) <$> fillRandom n
  if success
  then pure result
  else solveFor n
