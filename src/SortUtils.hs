module SortUtils (
  unsafeSortPool, sortPool, indCmpFit, indCmpCrowded, indCmpReverseDist
) where

import           Control.Monad.ST
import qualified Data.Vector                as V
import           Data.Vector.Algorithms.Tim as VA
import qualified Data.Vector.Unboxed        as VU

import           Genome


-- Compare two individuals
type IndCmp g = Ind g -> Ind g -> Ordering


-- Sort a pool of individuals with the given comparator in place.
-- Unsafe version that modifies the input vector
unsafeSortPool :: Pool g-> IndCmp g -> Pool g
unsafeSortPool pool cmp =
  runST $ do
    v <- V.unsafeThaw pool
    VA.sortBy cmp v
    V.unsafeFreeze v


-- Sort a pool of individuals with the given comparator in place.
-- Unsafe version that modifies the input vector
sortPool :: Pool g-> IndCmp g -> Pool g
sortPool pool cmp =
  runST $ do
    v <- V.thaw pool
    VA.sortBy cmp v
    V.unsafeFreeze v

-- Compare two individuals each with two fitnesses by increasing order of fitIdx
-- and for individuals with equal fitIdx, with increasing order of the other fitness
indCmpFit :: Int -> IndCmp g
indCmpFit fitIdx x y
  | a0 < b0 = LT
  | a0 > b0 = GT
  | a1 < b1 = LT
  | a1 > b1 = GT
  | a1 == b1 = EQ
  | otherwise = error "Exhausted indCmpFit"
    where
  a0 = fitnesses x VU.! f1
  a1 = fitnesses x VU.! f2
  b0 = fitnesses y VU.! f1
  b1 = fitnesses y VU.! f2
  f1 = if fitIdx == 0 then 0 else 1
  f2 = if fitIdx == 0 then 1 else 0


-- The 'crowded comparison' operator
indCmpCrowded :: IndCmp g
indCmpCrowded x y
  | r1 < r2 = LT
  | r1 > r2 = GT
  | d1 < d2 = LT
  | d1 > d2 = GT
  | d1 == d2 = EQ
  | otherwise = error "Exhausted indCmpCrowded"
    where
  r1 = rank x
  r2 = rank y
  d1 = cdist x
  d2 = cdist y


-- Descending crowding distance
indCmpReverseDist :: IndCmp g
indCmpReverseDist x y
  | d1 > d2 = LT
  | d1 < d2 = GT
  | d1 == d2 = EQ
  | otherwise = error "Exhausted indCmpReverseDist"
    where
  d1 = cdist x
  d2 = cdist y
