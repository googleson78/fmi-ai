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

module Xno where

import Control.Arrow ((&&&))
import Data.Traversable (for)
import Data.Vector (Vector, (!))
import qualified Data.Vector.Mutable as Mut (write, modify)
import qualified Data.Vector as Vec
import Data.Functor.Compose (Compose(..))
import Control.Monad (join)
import Data.Maybe (mapMaybe, listToMaybe)

data Spot = X | O
  deriving (Show, Eq, Ord)

opp :: Spot -> Spot
opp = \case
  X -> O
  O -> X

newtype Board a = Board {getBoard :: Vector (Vector (Maybe a))}
  deriving (Functor) via (Vector `Compose` Vector `Compose` Maybe)
  deriving newtype (Eq, Show, Ord)

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

maximiseOn :: Ord a => (a -> GameEnd) -> Rose a -> a
maximiseOn f = snd . maximumEnd f . evalMinimum f

evalMinimum :: Ord a => (a -> GameEnd) -> Rose a -> [a]
evalMinimum _ (Node x []) = [x]
evalMinimum f (Node _ xs) = minOmit1 f $ map (evalMaximum f) xs

evalMaximum :: Ord a => (a -> GameEnd) -> Rose a -> [a]
evalMaximum _ (Node x []) = [x]
evalMaximum f (Node _ xs) = maxOmit1 f $ map (evalMinimum f) xs

minOmit1 :: forall a. Ord a => (a -> GameEnd) -> [[a]] -> [a]
minOmit1 _ [] = error "shouldn't happen"
minOmit1 f (ys:yss) =
  let (base, move) = minimumEnd f ys
   in move : minOmit base yss
  where
    minOmit :: GameEnd -> [[a]] -> [a]
    minOmit _ [] = []
    minOmit curr (xs:xss) =
      if any ((<= curr) . f) xs
      then minOmit curr xss
      else
        let (newCurr, move) = minimumEnd f xs
         in move : minOmit newCurr xss

maxOmit1 :: forall a. Ord a => (a -> GameEnd) -> [[a]] -> [a]
maxOmit1 _ [] = error "shouldn't happen"
maxOmit1 f (ys:yss) =
  let (base, move) = maximumEnd f ys
   in move : maxOmit base yss
  where
    maxOmit :: GameEnd -> [[a]] -> [a]
    maxOmit _ [] = []
    maxOmit curr (xs:xss) =
      if any ((curr <=) . f) xs
      then maxOmit curr xss
      else
        let (newCurr, move) = maximumEnd f xs
         in move : maxOmit newCurr xss

-- Implement these manually to be as lazy as possible
minimumEnd :: Ord a => (a -> GameEnd) -> [a] -> (GameEnd, a)
minimumEnd f = go . map (f &&& id)
  where
    go [] = error "shouldn't happen"
    go [x] = x
    go ((Loss, res):_) = (Loss, res)
    go (x:xs) = min x $ go xs

maximumEnd :: Ord a => (a -> GameEnd) -> [a] -> (GameEnd, a)
maximumEnd f = go . map (f &&& id)
  where
    go [] = error "shouldn't happen"
    go [x] = x
    go ((Win, res):_) = (Win, res)
    go (x:xs) = max x $ go xs

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
  deriving (Show, Eq, Ord)

play :: Spot -> GameState -> (GameEnd, GameState)
play me startState = maximiseOn fst $ fmap (evalOnce me &&& id) $ allGames startState
