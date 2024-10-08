module Maze where

import Control.Lens
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.State
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Graph
import Random (shuffle)

-- | generate a maze by removing edges from its dual graph
generateMaze :: Graph -> Graph
generateMaze = undefined


type VisitedSet = IntSet
type Stack = [Vertex]

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
graphL f s = (\g -> s { graph = g }) <$> f s.graph

newS :: Graph -> S
newS g = S { visited = IntSet.empty, stack = [], graph = g }

type M m = (MonadRandom m, MonadState S m)

visit :: M m => Vertex -> m ()
visit v = visitedL %= IntSet.insert v

pop :: M m => m (Maybe Vertex)
pop = do
  stack <- use stackL
  case stack of
    [] -> pure Nothing
    (x:xs) -> stackL .= xs >> pure (Just x)

push :: M m => [Vertex] -> m ()
push vs = stackL %= (vs <>)

go :: M m => m ()
go = do
  next <- pop
  case next of
    Nothing -> pure ()
    Just v -> do
      visit v
      g <- use graphL
      visited <- use visitedL
      ns <- shuffle $ filter (flip IntSet.notMember visited) (neighbors v g)
      case ns of
        [] -> go
        (x:_) -> do
          graphL %= deleteEdge (v,x)
          push ns
          go

build :: IO Graph
build = do
  let g = squareGrid 5 5
  s' <- flip execStateT (newS g) (push [0] >> go)
  pure $ s'.graph




main :: IO ()
main = putStrLn "done"

