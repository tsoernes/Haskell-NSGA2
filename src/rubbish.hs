import           Control.Monad               as M
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import           System.Random


-- Create nvecs random vectors 
randVectors :: (RandomGen g) => Int -> Int -> g -> (V.Vector (VU.Vector Int), g)
randVectors nvecs veclen g =
  foldl step (V.empty, g) [0..nvecs]
    where
  step :: (RandomGen g) => (V.Vector (VU.Vector Int), g) -> Int -> (V.Vector (VU.Vector Int), g)
  step (li, g1) _ =
    (V.snoc li subli, g2)
      where
    (subli, g2) = randVector veclen g1


-- Create a vector containing the numbers [0,n) with length n randomly shuffled
randVector :: (RandomGen g) => Int -> g -> (VU.Vector Int, g)
randVector n =
  shuffle vector (VU.length vector)
    where
  vector = VU.enumFromN 0 n


-- Fischer-Yates shuffle. To shuffle a vector, pass its length as size parameter.
-- To get a random sample from a vector, pass the desired sample size as size.
shuffle :: (RandomGen g, VG.Vector v a) => v a -> Int -> g -> (v a, g)
shuffle vec size g =
  runST $ do
    vec_mut <- VG.thaw vec
    let swap_random g1 i = do
          let (j, g2) = randomR (0, i) g1
          VGM.swap vec_mut i j
          return g2
    g' <- M.foldM swap_random g [1..size-1]
    vec' <- VG.unsafeFreeze vec_mut
    let vec_sample = VG.take size vec'
    return (vec_sample, g')
