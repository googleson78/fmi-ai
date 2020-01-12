{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Conflicts
  ( Conflicts, mkConflicts
  , adjust
  , ix, (!), isConflicting
  ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as Mut

import Data.Hashable

newtype VecMap k = VecMap {getVecMap :: Vector Int}
  deriving Show

adjustVM :: Hashable k => (Int -> Int) -> k -> VecMap k -> VecMap k
adjustVM f k (VecMap v) = VecMap $ Vec.modify (\v' -> Mut.modify v' f $ hash k) v

ixVM :: Hashable k => k -> VecMap k -> Int
ixVM k (VecMap v) = v Vec.! hash k

data Conflicts = Conflicts
  { vertical :: VecMap Vertical
  , mainDiag :: VecMap MainDiag
  , revDiag :: VecMap RevDiag
  , size :: Int
  }
  deriving Show

mkConflicts :: Int -> Conflicts
mkConflicts n = Conflicts
  { vertical = VecMap $ Vec.replicate n 0
  , mainDiag = VecMap $ Vec.replicate ((2*n) - 1) 0
  , revDiag = VecMap $ Vec.replicate ((2*n) - 1) 0
  , size = n - 1
  }

adjust :: (Int -> Int) -> (Int, Int) -> Conflicts -> Conflicts
adjust f loc Conflicts{..} = Conflicts
  { vertical = adjustVM f (Vertical loc) vertical
  , mainDiag = adjustVM f (MainDiag $ attach size loc) mainDiag
  , revDiag = adjustVM f (RevDiag loc) revDiag
  , size
  }

attach :: a -> (b, c) -> (a, b, c)
attach n (x, y) = (n, x, y)

isConflicting :: (Int, Int) -> Conflicts -> Bool
isConflicting tup cs = ix tup cs > 3

ix :: (Int, Int) -> Conflicts -> Int
ix loc Conflicts{..}
  = ixVM (Vertical loc) vertical
  + ixVM (MainDiag $ attach size loc) mainDiag
  + ixVM (RevDiag loc) revDiag

(!) :: Conflicts -> (Int, Int) -> Int
(!) = flip ix

newtype Vertical = Vertical {getVertical :: (Int, Int)}
  deriving stock Show

instance Hashable Vertical where
  hashWithSalt _ = error "shouldn't be called"
  hash (Vertical (_, y)) = y
  {-# INLINE hash #-}

newtype RevDiag = RevDiag {getRevDiag :: (Int, Int)}
  deriving stock Show

instance Hashable RevDiag where
  hashWithSalt _ = error "shouldn't be called"
  hash (RevDiag (x, y)) = x + y
  {-# INLINE hash #-}

newtype MainDiag = MainDiag {getMainDiag :: (Int, Int, Int)}
  deriving stock Show

instance Hashable MainDiag where
  hashWithSalt _ = error "shouldn't be called"
  hash (MainDiag (n, x, y)) = n + x - y
  {-# INLINE hash #-}
