module RandUtils (
  randIndices, randVector, shuffle
) where

import           Control.Monad               as M
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import           Data.Vector.Unboxed         as VU
import           Data.Vector.Unboxed.Mutable as VUM
import           Prelude                     as P
import           System.Random

go = do
  g <- newStdGen
  let (rand_vecs, g1) = randVectors 5 10 g
  let (rand_vec, g2) = randVector 10 g1
  print rand_vecs
  print rand_vec


-- @TODO: there's probably something better than folds here
-- Create @nvecs@ unboxed vectors with length @veclen@, containing the numbers
-- 0 to @veclen@ exlusive randomly shuffled.
randVectors :: (RandomGen g) => Int -> Int -> g -> (V.Vector (VU.Vector Int), g)
randVectors nvecs veclen g = VU.foldl step (V.empty, g) $ VU.enumFromN 0 nvecs where
  step :: (RandomGen g) => (V.Vector (VU.Vector Int), g) -> Int -> (V.Vector (VU.Vector Int), g)
  step (li, g1) _ = (V.snoc li subli, g2) where
    (subli, g2) = randVector veclen g1


randVector :: (RandomGen g) => Int -> g -> (VU.Vector Int, g)
randVector n = shuffle vector (VU.length vector) where
  vector = VU.enumFromN 0 n


-- Fischer-Yates shuffle. To shuffle a vector, pass its length as size parameter.
-- To get a random sample from a vector, pass the desired sample size as size.
shuffle :: (RandomGen g, VG.Vector v a) => v a -> Int -> g -> (v a, g)
shuffle li size g = runST $ do
  vector <- VG.thaw li
  let n = VGM.length vector - 1
  let swap_random g1 i = do
        let (j,g2) = randomR (0,n) g1
        VGM.swap vector i j
        return g2
  g' <- M.foldM swap_random g [0..size-1]
  v' <- VG.unsafeFreeze vector
  let vec = VG.take size v'
  return (vec, g')


randIndices :: RandomGen g => Int -> g -> (Int, Int, g)
randIndices len g =
  (i_left, i_right, g2)
    where
  (i_left, g1) = randomR (0, len-1) g
  (i_right, g2) = randomR (i_left+1, len) g1


randVector' :: Int -> IO (VU.Vector Int)
randVector' n = do
  vector <- VU.unsafeThaw (VU.enumFromN 1 n)
  M.forM_ [1..VUM.length vector-1] $ \i -> do
    j <- randomRIO(0, i) :: IO Int
    VUM.swap vector i j
  VU.unsafeFreeze vector
