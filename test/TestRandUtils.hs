module TestRandUtils (
) where

import           Control.Monad               as M
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import           Data.Vector.Unboxed.Mutable as VUM
import           System.Random


randVectors :: (RandomGen g) => Int -> Int -> g -> (V.Vector (VU.Vector Int), g)
randVectors = undefined


randVector :: (RandomGen g) => Int -> g -> (VU.Vector Int, g)
randVector = undefined


shuffle :: (RandomGen g, VG.Vector v a) => v a -> Int -> g -> (v a, g)
shuffle = undefined


randIndices :: RandomGen g => Int -> g -> (Int, Int, g)
randIndices = undefined


randVector' :: Int -> IO (VU.Vector Int)
randVector' = undefined
