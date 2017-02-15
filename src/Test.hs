module Test (
) where

import           Control.Monad
import           Control.Monad.Random as MR
import           Control.Monad.ST
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as VG
import qualified Data.Vector.Unboxed  as VU
import Genome

-- parent_a = [1, 4, 2, 8, 5, 7, 3, 6, 9]
-- parent_b = [7, 5, 3, 1, 9, 8, 6, 4, 2]
-- left = 3, right = 7
-- c_a_mid  = [8, 5, 7, 3]
-- c_a_miss = [1, 9, 6, 4, 2]
-- tlb      = take left parent_b =
-- drb      = drop right parent_b =
-- c_a      = tlb ++ c_a_mid ++ drb
-- c_a      = [1, 9, 6, 8, 5, 7, 3, 4, 2]

{-
oc :: IO()
oc = do
  let parent_a = VU.fromList [1::Int, 4, 2, 8, 5, 7, 3, 6, 9]
      parent_b = VU.fromList [7::Int, 5, 3, 1, 9, 8, 6, 4, 2]
      (left, right) = (3, 7)
      n = right - left
      c_a_mid = VG.slice left n parent_a
      c_b_mid = VG.slice left n parent_b
      c_a_miss = VG.filter (`VG.notElem` c_a_mid) (VG.drop right parent_b VG.++ VG.take right parent_b)
      c_b_miss = VG.filter (`VG.notElem` c_b_mid) (VG.drop right parent_a VG.++ VG.take right parent_a)
      splitpos = (VG.length parent_a) - right
      (ra, la) = VG.splitAt splitpos c_a_miss
      (rb, lb) = VG.splitAt splitpos c_b_miss

  print $ "child_a_miss OK: " ++  show (c_a_miss == VU.fromList [4::Int, 2, 1, 9, 6])
  print c_a_miss
  print $ "child_b_miss OK: " ++  show (c_b_miss == VU.fromList [4::Int, 2, 5, 7, 3])
  print c_b_miss -- OK

  let child_a = la VG.++ c_a_mid VG.++ ra
      child_b = lb VG.++ c_b_mid VG.++ rb

  print $ "child_a OK: " ++  show (child_a == VU.fromList [1::Int, 9, 6, 8, 5, 7, 3, 4, 2])
  print child_a
  print $ "child_b OK: " ++  show (child_b == VU.fromList [5::Int, 7, 3, 1, 9, 8, 6, 4, 2])
  print child_b
-}

import           Control.Monad
import           Control.Monad.Random
import qualified Data.Vector.Unboxed  as VU

testReprod :: IO()
testReprod = do
  let parents = VU.fromList [1::Int,2,3,4,5,6,7,8,9,10]
  children <- reproduce parents
  print children

-- concat monadic
reproduce :: (MonadRandom m) => Pool -> m Pool
reproduce parents = vectorConcatMapM mate mates
    where
  half = VU.length parents `div` 2
  mates = VU.zip (VU.take half parents) (VU.drop half parents)
  mate :: (MonadRandom m) => (Ind, Ind) -> m Pool
  mate (a, b) = do
    r1 <- getRandomR(0,5)
    r2 <- getRandomR(0,5)
    return $ VU.fromList [a+r1, b+r2]

vectorConcatMapM :: Monad m => (a -> m (V.Vector b)) -> V.Vector a -> m (V.Vector b)
vectorConcatMapM f v = join <$> sequence (fmap f v)
