{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module HashAbuse
  ( Conflicts, mkConflicts
  , adjust
  , ix, (!)
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Hashable
import Data.Bifunctor (first)

data Conflicts = Conflicts
  { vertical :: {-# UNPACK #-} !(HashMap Vertical Int)
  , mainDiag :: {-# UNPACK #-} !(HashMap MainDiag Int)
  , revDiag :: {-# UNPACK #-} !(HashMap RevDiag Int)
  }
  deriving Show

mkConflicts :: Int -> Conflicts
mkConflicts size = Conflicts
  { vertical = Map.fromList $ map (first Vertical) allTups
  , mainDiag = Map.fromList $ map (first MainDiag) allTups
  , revDiag = Map.fromList $ map (first RevDiag) allTups
  }
  where
    allTups = flip zip (repeat 0) [(x, y) | x <- [0..size - 1], y <- [0..size - 1]]

adjust :: (Int -> Int) -> (Int, Int) -> Conflicts -> Conflicts
adjust f loc old = Conflicts
  { vertical = Map.adjust f (Vertical loc) $ vertical old
  , mainDiag = Map.adjust f (MainDiag loc) $ mainDiag old
  , revDiag = Map.adjust f (RevDiag loc) $ revDiag old
  }

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

newtype RevDiag = RevDiag {getRevDiag :: (Int, Int)}
  deriving stock Show
  deriving Eq via EqOnHash RevDiag

instance Hashable RevDiag where
  hashWithSalt _ = error "shouldn't be called"
  hash (RevDiag (x, y)) = x + y

newtype MainDiag = MainDiag {getMainDiag :: (Int, Int)}
  deriving stock Show
  deriving Eq via EqOnHash MainDiag

instance Hashable MainDiag where
  hashWithSalt _ = error "shouldn't be called"
  hash (MainDiag (x, y)) = x - y

newtype EqOnHash a = EqOnHash a

instance Hashable a => Eq (EqOnHash a) where
  EqOnHash x == EqOnHash y = hash x == hash y
