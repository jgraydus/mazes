module Maze (
    Maze(..), generateMaze
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

data Maze = Maze
  { width :: Int
  , height :: Int
  , graph :: Graph
  }

generateMaze :: (MonadRandom m, MonadWriter [Graph] m) => Int -> Int -> m Maze
generateMaze w h = do
  g <- build w h
  pure $ Maze { width = w, height = h, graph = g }

type VisitedSet = IntSet
type Stack = [Edge]

data S = S
  { visited :: VisitedSet
  , stack :: Stack
  , graph :: Graph
  }

visitedL :: Lens' S VisitedSet
visitedL f s = (\v -> s { visited = v }) <$> f s.visited

stackL :: Lens' S Stack
stackL f s = (\st -> s { stack = st }) <$> f s.stack

graphL :: Lens' S Graph
graphL f s = (\g -> s { graph = g } :: S) <$> f s.graph

newS :: Graph -> S
newS g = S { visited = IntSet.empty, stack = [], graph = g }

type M m = (MonadRandom m, MonadState S m, MonadWriter [Graph] m)

visit :: M m => Vertex -> m ()
visit v = visitedL %= IntSet.insert v

pop :: M m => m (Maybe Edge)
pop = do
  stack <- use stackL
  case stack of
    [] -> pure Nothing
    (x:xs) -> stackL .= xs >> pure (Just x)

push :: M m => [Edge] -> m ()
push vs = stackL %= (vs <>)

snapshot :: M m => m ()
snapshot = do
  g <- use graphL
  tell [g]

go :: M m => m ()
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

build :: (MonadRandom m, MonadWriter [Graph] m) => Int -> Int -> m Graph
build w h = do
  let g = squareGrid w h
  start <- getRandomR (0, w*h-1)
  to_ <- fmap head <$> shuffle $ neighbors start g
  s' <- flip execStateT (newS g) (visit start >> push [(start, to_)] >> go)
  pure $ s'.graph

