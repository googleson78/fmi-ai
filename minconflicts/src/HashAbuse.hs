{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module HashAbuse where

import Data.Hashable
import Data.Function (on)

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

newtype EqOnHash a = EqOnHash {getEqOnHash :: a}

instance Hashable a => Eq (EqOnHash a) where
  (==) = (==) `on` (hash . getEqOnHash)
