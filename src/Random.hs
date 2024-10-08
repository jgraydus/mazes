module Random where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Random.Class (MonadRandom(..))
import Data.Vector.Generic.Mutable qualified as MV
import Data.Vector.Mutable (MVector)

swap :: MVector s a -> (Int, Int) -> ST s ()
swap v (i, j) = do
  iv <- MV.read v i
  jv <- MV.read v j
  MV.write v i jv
  MV.write v j iv

-- | create a uniform random permutation (see Fisher-Yates)
randomPermutation :: MonadRandom m => Int -> m [Int]
randomPermutation n = shuffle [0..n-1]

-- | shuffle a given list via a uniform random permutation
shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
  let n = length xs
  ps <- forM [0..n-2] $ \m -> getRandomR (m, n-1)
  pure $ runST $ do
    v :: MVector s a <- MV.new n
    forM_ (zip [0..] xs) $ \(i, x) -> MV.write v i x
    forM_ (zip [0..] ps) $ swap v
    MV.foldr' (:) [] v

