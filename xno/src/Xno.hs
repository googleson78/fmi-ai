{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
--
-- {-# OPTIONS_GHC -funbox-strict-fields #-}

module Xno where

import Data.Traversable (for)
import Data.Vector (Vector, (!))
import qualified Data.Vector.Mutable as Mut (write, modify)
import qualified Data.Vector as Vec
import Data.Functor.Compose (Compose(..))
import Control.Monad (join)
import Data.Maybe (mapMaybe, listToMaybe)

data Spot = X | O
  deriving (Show, Eq)

opp :: Spot -> Spot
opp = \case
  X -> O
  O -> X

newtype Board a = Board {getBoard :: Vector (Vector (Maybe a))}
  deriving (Functor) via (Vector `Compose` Vector `Compose` Maybe)
  deriving newtype (Eq, Show)

data Status a
  = InProgress
  | Draw
  | Winner a


winner :: forall a. (Eq a) => Board a -> Status a
winner board@(Board vss) =
  if all (all (not . null)) vss
  then case winnerEnd board of
         Nothing -> Draw
         Just x -> Winner x
  else InProgress

-- Assume board is full;
-- Nothing == Draw
winnerEnd :: forall a. (Eq a) => Board a -> Maybe a
winnerEnd (Board vss) = listToMaybe $ concatMap (mapMaybe checkIndices) [rows, cols, diags]
  where
    checkIndices :: [(Int, Int)] -> Maybe a
    checkIndices indices = maybe Nothing allSame $ for indices \(x, y) -> vss ! x ! y

    allSame :: [a] -> Maybe a
    allSame [] = error "you dum-dum"
    allSame (x:xs) = if all (==x) xs then Just x else Nothing

    rows :: [[(Int, Int)]]
    rows = boardIndices

    cols :: [[(Int, Int)]]
    cols = map (map (\(x,y) -> (y,x))) boardIndices

    diags :: [[(Int, Int)]]
    diags = [diag, revDiag]

    diag :: [(Int, Int)]
    diag = join zip [0..size]

    revDiag :: [(Int, Int)]
    revDiag = zip [size,size - 1..0] [0..size]

    boardIndices :: [[(Int, Int)]]
    boardIndices = map (\x -> map (x,) [0..size]) [0..size]

    size :: Int
    size = Vec.length vss - 1

allEmpty :: Eq a => Board a -> [(Int, Int)]
allEmpty (Board vss) = do
  n <- [0..length vss - 1]
  map (n,) $ Vec.toList $ Vec.findIndices (== Nothing) $ vss ! n

place :: a -> Board a -> (Int, Int) -> Board a
place x (Board vss) (i, j) = Board $
  Vec.modify (\vss' -> Mut.modify vss' (\vs -> Vec.modify (\vs' -> Mut.write vs' j $ Just x) vs) i) vss

next :: GameState -> [GameState]
next GameState{..} = map (GameState (opp currPlayer) . place currPlayer currBoard) $ allEmpty currBoard

board0 :: Board Spot
board0 = Board $ Vec.fromList $ map (Vec.fromList)
  [ [Nothing, Just O, Just O]
  , [Nothing, Just O, Nothing]
  , [Nothing, Just X, Nothing]
  ]

board1 :: Board Spot
board1 = Board $ Vec.fromList $ map (Vec.fromList)
  [ [Just O, Just O]
  , [Nothing, Just O]
  ]

data GameEnd = Loss | Drawn | Win
  deriving (Show, Eq)

instance Ord GameEnd where
  Loss <= _ = True
  Drawn <= Loss = False
  Drawn <= _ = True
  Win <= _ = False

evalOnce :: Spot -> GameState -> GameEnd
evalOnce me GameState{..} = case winner currBoard of
  Winner x -> if x == me then Win else Loss
  _ -> Drawn

data Rose a = Node a [Rose a]
  deriving stock (Show, Functor)

instance Foldable Rose where
  foldMap f (Node x xs) = f x <> foldMap (foldMap f) xs

data MinOrMax = Min | Max

flipMM :: MinOrMax -> MinOrMax
flipMM = \case
  Min -> Max
  Max -> Min

flipGameEnd :: GameEnd -> GameEnd
flipGameEnd = \case
  Loss -> Win
  Drawn -> Drawn
  Win -> Loss

maximise :: Rose GameEnd -> GameEnd
maximise (Node x []) = x
maximise (Node _ xs) = maximumEnd $ map minimise xs

minimise :: Rose GameEnd -> GameEnd
minimise (Node x []) = x
minimise (Node _ xs) = minimumEnd $ map maximise xs

-- Implement these manually to be as lazy as possible
minimumEnd :: [GameEnd] -> GameEnd
minimumEnd [] = Win
minimumEnd (Loss:_) = Loss
minimumEnd (x:xs) = min x $ minimumEnd xs

maximumEnd :: [GameEnd] -> GameEnd
maximumEnd [] = Loss
maximumEnd (Win:_) = Win
maximumEnd (x:xs) = max x $ maximumEnd xs

iterateRose :: (a -> [a]) -> a -> Rose a
iterateRose f x = Node x $ map (iterateRose f) $ f x

cutOff :: Int -> Rose a -> Rose a
cutOff 0 (Node x _) = Node x []
cutOff n (Node x xs) = Node x $ map (cutOff (n - 1)) xs

allGames :: GameState -> Rose GameState
allGames = iterateRose next

data GameState = GameState
  { currPlayer :: Spot
  , currBoard :: Board Spot
  }
  deriving Show

start :: GameState
start = GameState
  { currPlayer = X
  , currBoard = Board $ Vec.replicate 3 $ Vec.replicate 3 Nothing
  }

play :: Spot -> GameState -> GameEnd
play me startState = extremise $ fmap (evalOnce me) $ allGames startState
  where
    extremise = case me of
      X -> maximise
      O -> minimise
