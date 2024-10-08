module Random where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Random.Class (MonadRandom(..))
import Data.Vector.Generic.Mutable qualified as MV
import Data.Vector.Mutable (MVector)

-- | create a uniform random permutation
randomPermutation :: MonadRandom m => Int -> m [Int]
randomPermutation n = shuffle [0..n-1]

-- | shuffle a given list via a uniform random permutation (see Fisher-Yates algorithm)
shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
  let n = length xs
  ps <- forM [0..n-2] $ \m -> getRandomR (m, n-1)
  pure $ runST $ do
    v :: MVector s a <- MV.new n
    forM_ (zip [0..] xs) $ \(i, x) -> MV.unsafeWrite v i x
    forM_ (zip [0..] ps) $ uncurry (MV.unsafeSwap v)
    MV.foldr' (:) [] v

