{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Conflicts
  ( Conflicts, mkConflicts
  , adjust
  , ix, (!), isConflicting
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Hashable
import Data.Bifunctor (first)

data Conflicts = Conflicts
  { vertical :: !(HashMap Vertical Int)
  , mainDiag :: !(HashMap MainDiag Int)
  , revDiag :: !(HashMap RevDiag Int)
  }
  deriving Show

mkConflicts :: Int -> Conflicts
mkConflicts size = Conflicts
  { vertical = Map.fromList $ map (first Vertical) allTups
  , mainDiag = Map.fromList $ map (first MainDiag) allTups
  , revDiag = Map.fromList $ map (first RevDiag) allTups
  }
  where
    n = size - 1
    ixs = [0..n]
    edges
      =  [(x, 0) | x <- ixs]
      ++ [(0, x) | x <- ixs]
      ++ [(n, x) | x <- ixs]
      ++ [(x, n) | x <- ixs]
    allTups = flip zip (repeat 0) edges

adjust :: (Int -> Int) -> (Int, Int) -> Conflicts -> Conflicts
adjust f loc old = Conflicts
  { vertical = Map.adjust f (Vertical loc) $ vertical old
  , mainDiag = Map.adjust f (MainDiag loc) $ mainDiag old
  , revDiag = Map.adjust f (RevDiag loc) $ revDiag old
  }

isConflicting :: (Int, Int) -> Conflicts -> Bool
isConflicting tup cs = ix tup cs > 3

ix :: (Int, Int) -> Conflicts -> Int
ix loc Conflicts{..}
  = vertical Map.! Vertical loc
  + mainDiag Map.! MainDiag loc
  + revDiag Map.! RevDiag loc

(!) :: Conflicts -> (Int, Int) -> Int
(!) = flip ix

newtype Vertical = Vertical {getVertical :: (Int, Int)}
  deriving stock Show
  deriving Eq via EqOnHash Vertical

instance Hashable Vertical where
  hashWithSalt _ = error "shouldn't be called"
  hash (Vertical (_, y)) = y
  {-# INLINE hash #-}

newtype RevDiag = RevDiag {getRevDiag :: (Int, Int)}
  deriving stock Show
  deriving Eq via EqOnHash RevDiag

instance Hashable RevDiag where
  hashWithSalt _ = error "shouldn't be called"
  hash (RevDiag (x, y)) = x + y
  {-# INLINE hash #-}

newtype MainDiag = MainDiag {getMainDiag :: (Int, Int)}
  deriving stock Show
  deriving Eq via EqOnHash MainDiag

instance Hashable MainDiag where
  hashWithSalt _ = error "shouldn't be called"
  hash (MainDiag (x, y)) = x - y
  {-# INLINE hash #-}

newtype EqOnHash a = EqOnHash a

instance Hashable a => Eq (EqOnHash a) where
  EqOnHash x == EqOnHash y = hash x == hash y
  {-# INLINE (==) #-}
