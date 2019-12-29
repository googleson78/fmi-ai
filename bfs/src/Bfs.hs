{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Bfs (Board(..), BfsState(..), bfs) where

import Prelude hiding (Either(..))
import Control.Monad.Extra (unlessM, whileM)
import Data.Functor ((<&>))
import Data.Foldable (for_, traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Control.Monad.State.Class (MonadState(..), gets, modify')
import Control.Monad.Reader.Class (MonadReader(..), asks)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq((:<|), Empty), (|>))
import Data.Generics.Product (field)
import Lens.Micro (over, set)
import GHC.Generics (Generic)

newtype Board a = Board {getBoard :: Vector (Vector a)}
  deriving Functor via (Vector `Compose` Vector)
  deriving newtype (Eq, Show, Ord)

ix :: Int -> Int -> Board a -> a
ix x y Board {..} = getBoard ! x ! y

data Spot
  = Blocked
  | Normal
  | Portal Location
  deriving (Show, Read)

type Location = (Int, Int)

data BfsState = BfsState
  { visitedNodes :: HashSet Location
  , queue :: Seq Location
  , paths :: HashMap Location Location
  }
  deriving stock Generic

isVisited :: (MonadState BfsState m) => Location -> m Bool
isVisited loc = gets $ Set.member loc . visitedNodes

markVisited :: (MonadState BfsState m) => Location -> m ()
markVisited = modify' . over (field @"visitedNodes") . Set.insert

enqueue :: (MonadState BfsState m) => Location -> m ()
enqueue loc = modify' $ over (field @"queue") (|> loc)

markParent :: (MonadState BfsState m) => Location -> Location -> m ()
markParent loc parentLoc = modify' $ over (field @"paths") $ (`Map.alter` loc) \case
  Nothing -> Just parentLoc
  x -> x

enqueueNeighbours
  :: (MonadReader (Board Spot) m, MonadState BfsState m)
  => Location -> m ()
enqueueNeighbours loc = for_ dirs \direction ->
  go direction loc >>=
    traverse_ \newLoc -> unlessM (isVisited newLoc) do
      spotType newLoc >>= \case
        Normal -> enqueue newLoc
        Blocked -> pure ()
        Portal portalLoc -> do
          markParent portalLoc newLoc
          enqueue portalLoc
      markParent newLoc loc

bfs
  :: (MonadReader (Board Spot) m, MonadState BfsState m)
  => Location -> m ()
bfs start = do
  enqueue start
  markVisited start
  whileM $ do
    BfsState{queue} <- get
    case queue of
      Empty -> pure False
      curr :<| rest -> do
        modify' $ set (field @"queue") rest
        enqueueNeighbours curr
        markVisited curr
        pure True

data Direction
  = Up
  | Right
  | Down
  | Left

go
  :: MonadReader (Board a) m
  => Direction -> Location -> m (Maybe Location)
go dir (x, y) = do
  Board {..} <- ask
  let newLoc = case dir of
        Up -> (x - 1, y)
        Left -> (x, y + 1)
        Down -> (x + 1, y)
        Right -> (x, y - 1)
  inBounds newLoc <&> \case
    True -> Just $ newLoc
    False -> Nothing

spotType
  :: MonadReader (Board Spot) m
  => Location -> m Spot
spotType = asks . uncurry ix

inBounds
  :: MonadReader (Board a) m
  => Location -> m Bool
inBounds (x, y) = do
  Board {..} <- ask
  pure $  0 <= x && x < length getBoard
       && 0 <= y && y < length (Vec.head getBoard)

dirs :: [Direction]
dirs = [Up, Right, Down, Left]
