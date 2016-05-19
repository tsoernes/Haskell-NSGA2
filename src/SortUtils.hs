module SortUtils (
  sortPool, indCmpFit, indCmpCrowded, indCmpReverseDist
) where

import           Control.Monad.ST
import qualified Data.Vector                as V
import           Data.Vector.Algorithms.Tim as VA
import qualified Data.Vector.Unboxed        as VU

import           Genome


type IndCmp =
  Ind -> Ind -> Ordering


sortPool :: Pool-> IndCmp -> Pool
sortPool pool cmp =
  runST $ do
    v <- V.unsafeThaw pool
    VA.sortBy cmp v
    V.unsafeFreeze v


-- Comparator to sort a list of individuals by increasing order of fitIdx and
-- for individuals with equal fitIdx, with increasing order of the other fitness
indCmpFit :: Int -> IndCmp
indCmpFit fitIdx x y
  | a0 < b0 = LT
  | a0 > b0 = GT
  | a1 < b1 = LT
  | a1 > b1 = GT
  | a1 == b1 = EQ
    where
  f1 = if fitIdx == 0 then 0 else 1
  f2 = if fitIdx == 0 then 1 else 0
  a0 = fitnesses x VU.! f1
  a1 = fitnesses x VU.! f2
  b0 = fitnesses y VU.! f1
  b1 = fitnesses y VU.! f2


-- The 'crowded comparison' operator
indCmpCrowded :: IndCmp
indCmpCrowded x y
  | r1 < r2 = LT
  | r1 > r2 = GT
  | d1 < d2 = LT
  | d1 > d2 = GT
  | d1 == d2 = EQ
    where
  r1 = rank x
  r2 = rank y
  d1 = cdist x
  d2 = cdist y


-- Descending crowding distance
indCmpReverseDist :: IndCmp
indCmpReverseDist x y
  | d1 > d2 = LT
  | d1 < d2 = GT
  | d1 == d2 = EQ
    where
  d1 = cdist x
  d2 = cdist y