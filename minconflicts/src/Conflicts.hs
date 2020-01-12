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

adjustVM :: (Int -> Int) -> Int -> Vector Int -> Vector Int
adjustVM f k v = Vec.modify (\v' -> Mut.modify v' f k) v

data Conflicts = Conflicts
  { vertical :: Vector Int
  , mainDiag :: Vector Int
  , revDiag :: Vector Int
  , size :: Int
  }
  deriving Show

mkConflicts :: Int -> Conflicts
mkConflicts n = Conflicts
  { vertical = Vec.replicate n 0
  , mainDiag = Vec.replicate ((2*n) - 1) 0
  , revDiag = Vec.replicate ((2*n) - 1) 0
  , size = n - 1
  }

adjust :: (Int -> Int) -> (Int, Int) -> Conflicts -> Conflicts
adjust f loc Conflicts{..} = Conflicts
  { vertical = adjustVM f (translateVertical loc) vertical
  , mainDiag = adjustVM f (translateMainDiag size loc) mainDiag
  , revDiag = adjustVM f (translateRevDiag loc) revDiag
  , size
  }

isConflicting :: (Int, Int) -> Conflicts -> Bool
isConflicting tup cs = ix tup cs > 3

ix :: (Int, Int) -> Conflicts -> Int
ix loc Conflicts{..}
  = vertical `Vec.unsafeIndex` translateVertical loc
  + mainDiag `Vec.unsafeIndex` translateMainDiag size loc
  + revDiag `Vec.unsafeIndex` translateRevDiag loc

(!) :: Conflicts -> (Int, Int) -> Int
(!) = flip ix

translateVertical :: (a, b) -> b
translateVertical (_, y) = y
{-# INLINE translateVertical #-}

translateRevDiag :: (Int, Int) -> Int
translateRevDiag (x, y) = x + y
{-# INLINE translateRevDiag #-}

translateMainDiag :: Int -> (Int, Int) -> Int
translateMainDiag n (x, y) = n + x - y
{-# INLINE translateMainDiag #-}
