module RandUtils (
  randIndices, randVector, shuffle
) where

import           Control.Monad               as M
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import           Data.Vector.Unboxed.Mutable as VUM
import           System.Random
{-
go :: IO()
go = do
  g <- newStdGen
  let (rand_vecs, g1) = randVectors 5 10 g
  let (rand_vec, g2) = randVector 10 g1
  print rand_vecs
  print rand_vec
-}
--foldl :: (a -> b -> a) -> a -> Vector b -> a
--prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
--scanl :: (a -> b -> a) -> a -> Vector b -> Vector a

-- @TODO: there's probably something better than folds here
-- Create @nvecs@ unboxed vectors with length @veclen@, each containing the
-- numbers 0 to @veclen@ exlusive, randomly shuffled.
randVectors :: (RandomGen g) => Int -> Int -> g -> (V.Vector (VU.Vector Int), g)
randVectors nvecs veclen g =
  foldl step (V.empty, g) [0..nvecs]
    where
  step :: (RandomGen g) => (V.Vector (VU.Vector Int), g) -> Int -> (V.Vector (VU.Vector Int), g)
  step (li, g1) _ =
    (V.snoc li subli, g2)
      where
    (subli, g2) = randVector veclen g1


randVector :: (RandomGen g) => Int -> g -> (VU.Vector Int, g)
randVector n =
  shuffle vector $ VU.length vector
    where
  vector = VU.enumFromN 0 n


-- Fischer-Yates shuffle. To shuffle a vector, pass its length as size parameter.
-- To get a random sample from a vector, pass the desired sample size as size.
shuffle :: (RandomGen g, VG.Vector v a) => v a -> Int -> g -> (v a, g)
shuffle vec size g =
  runST $ do
    vec_mut <- VG.thaw vec
    let swap_random g1 i = do
          let (j,g2) = randomR (0,i) g1
          VGM.swap vec_mut i j
          return g2
    g' <- M.foldM swap_random g [1..size-1]
    vec' <- VG.unsafeFreeze vec_mut
    let vec_sample = VG.take size vec'
    return (vec_sample, g')


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
