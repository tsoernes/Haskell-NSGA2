module RandUtils (
  randIndices, randVector, randVectors, randShuffle, randSample
) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU


-- | Create 'nvecs' unboxed vectors of length 'veclen', each containing the
-- numbers 0 to 'veclen', randomly shuffled.
randVectors :: (MonadRandom m) => Int -> Int ->  m (V.Vector (VU.Vector Int))
randVectors n len = V.replicateM n (randVector len)


-- | Get a vector of length 'n', containing the numbers 0 to 'n', randomly shuffled
randVector :: (MonadRandom m) => Int -> m (VU.Vector Int)
randVector len = randShuffle (VU.enumFromN 0 len)


-- | Get two distinct 'Ints' in the interval 0 through 'len', with the lowest
-- number appearing first in the tuple
randIndices :: (MonadRandom m) => Int -> m (Int, Int)
randIndices len = do
  i_left <- getRandomR (0, len-1)
  i_right <- getRandomR (i_left+1, len)
  return (i_left, i_right)


-- | Shuffle a vector randomly
randShuffle :: (MonadRandom m, (VG.Vector v a)) => v a -> m (v a)
randShuffle vec = randSample vec $ VG.length vec


-- | Fisher-Yates shuffle. To shuffle a 'vector', pass its 'length'.
-- To get a random sample from a vector, pass the desired sample size as 'length'.
randSample :: (MonadRandom m, (VG.Vector v a)) => v a -> Int -> m (v a)
randSample vec len = do
  let getR i = do
          r <- getRandomR (i, VG.length vec - 1)
          return (i, r)

  swaps <- mapM getR [0..len-1]

  let vec_rands = runST $ do
          vec_mut <- VG.thaw vec
          forM_ swaps $ uncurry (VGM.swap vec_mut)
          VG.unsafeFreeze vec_mut

  return $ VG.take len vec_rands
