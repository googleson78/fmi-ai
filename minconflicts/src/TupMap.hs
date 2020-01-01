{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module TupMap
  ( TupMap
  , mkTupMap
  , adjust, adjustBulk
  , ix, (!)
  , mapWithKey
  , toList
  ) where

import Data.Foldable (for_)
import Data.Bifunctor (first)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as Mut

-- a ((Int, Int) -> a) map as an associative vector,
--
-- no bounds checking is performed
data TupMap a = TupMap
  { sizeY :: {-# UNPACK #-} !Int
  , getTupMap :: !(Vector a)
  }
  deriving Show

mkTupMap :: Mut.Unbox a => Int -> Int -> a -> TupMap a
mkTupMap sizeX sizeY defaultVal =
  let getTupMap = Vec.fromList $ replicate (sizeX * sizeY) defaultVal
   in TupMap {..}
{-# INLINE mkTupMap #-}

adjust :: Mut.Unbox a => (a -> a) -> (Int, Int) -> TupMap a -> TupMap a
adjust f tup old@TupMap{getTupMap, sizeY} =
  old {getTupMap = Vec.modify (\mv -> Mut.modify mv f $ translate sizeY tup) getTupMap}
{-# INLINE adjust #-}

adjustBulk :: Mut.Unbox a => (a -> a) -> [(Int, Int)] -> TupMap a -> TupMap a
adjustBulk f tups old@TupMap{getTupMap, sizeY} =
  old {getTupMap = Vec.modify
        (\mv -> for_ tups $ Mut.modify mv f . translate sizeY)
        getTupMap}
{-# INLINE adjustBulk #-}

ix :: Mut.Unbox a => (Int, Int) -> TupMap a -> a
ix tup TupMap {getTupMap, sizeY} = getTupMap Vec.! (translate sizeY tup)
{-# INLINE ix #-}

mapWithKey
  :: (Mut.Unbox a, Mut.Unbox b)
  => ((Int, Int) -> a -> b)
  -> TupMap a
  -> TupMap b
mapWithKey f old@TupMap {getTupMap, sizeY} =
  old {getTupMap = Vec.imap (liftTranslate sizeY f) getTupMap}
{-# INLINE mapWithKey #-}

translate :: Int -> (Int, Int) -> Int
translate sizeY (i, j) = i * sizeY + j
{-# INLINE translate #-}

liftTranslate :: Int -> ((Int, Int) -> a) -> (Int -> a)
liftTranslate sizeY f n = f $ untranslate sizeY n
{-# INLINE liftTranslate #-}

untranslate :: Int -> Int -> (Int, Int)
untranslate = flip divMod
{-# INLINE untranslate #-}

(!) :: Mut.Unbox a => TupMap a -> (Int, Int) -> a
(!) = flip ix
{-# INLINE (!) #-}

toList :: Mut.Unbox a => TupMap a -> [((Int, Int), a)]
toList TupMap{getTupMap, sizeY}
  = map (first $ untranslate sizeY)
  $ Vec.toList
  $ Vec.indexed getTupMap
{-# INLINE toList #-}
