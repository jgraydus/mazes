module Maze (
    CellShape(..),
    generateMaze,
    Maze(..)
) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Random.Class (getRandomR, MonadRandom)
import Control.Monad.State
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Graph
import Random (shuffle)

data CellShape = Square | Hex
  deriving Show

mkGrid :: CellShape -> Int -> Int -> Graph
mkGrid Square = squareGrid
mkGrid Hex = hexGrid

data Maze = Maze
  { width :: Int
  , height :: Int
  , graph :: Graph
  , cellShape :: CellShape
  } deriving Show

generateMaze :: (MonadRandom m, MonadWriter [Graph] m) => CellShape -> Int -> Int -> m Maze
generateMaze cellShape w h = do
  g <- build cellShape w h
  pure $ Maze { width = w, height = h, graph = g, cellShape }

type VisitedSet = IntSet
type Stack = [Edge]

data S = S
  { visited :: VisitedSet
  , stack :: Stack
  , graph :: Graph
  }

newS :: Graph -> S
newS g = S { visited = IntSet.empty, stack = [], graph = g }

visitedL :: Lens' S VisitedSet
visitedL f s = (\v -> s { visited = v }) <$> f s.visited

stackL :: Lens' S Stack
stackL f s = (\st -> s { stack = st }) <$> f s.stack

graphL :: Lens' S Graph
graphL f s = (\g -> s { graph = g } :: S) <$> f s.graph

build :: (MonadRandom m, MonadWriter [Graph] m) => CellShape -> Int -> Int -> m Graph
build cellShape w h = do
  let g = mkGrid cellShape w h
  start <- getRandomR (0, w*h-1)
  to_ <- fmap head <$> shuffle $ neighbors start g
  s' <- flip execStateT (newS g) (visit start >> push [(start, to_)] >> go)
  pure $ s'.graph

  where
    go :: (MonadRandom m, MonadState S m, MonadWriter [Graph] m) => m ()
    go = do
      next <- pop
      case next of
        Nothing -> pure ()
        Just e@(_, v) -> do
          visited <- use visitedL
          when (IntSet.notMember v visited) $ do
            graphL %= deleteEdge e
            snapshot
            visit v
            g <- use graphL
            ns <- shuffle $ neighbors v g
            push $ fmap (v,) ns
          go

    visit :: MonadState S m => Vertex -> m ()
    visit v = visitedL %= IntSet.insert v

    push :: MonadState S m => [Edge] -> m ()
    push vs = stackL %= (vs <>)

    pop :: MonadState S m => m (Maybe Edge)
    pop = do
      stack <- use stackL
      case stack of
        [] -> pure Nothing
        (x:xs) -> stackL .= xs >> pure (Just x)

    snapshot :: (MonadState S m, MonadWriter [Graph] m) => m ()
    snapshot = do
      g <- use graphL
      tell [g]

