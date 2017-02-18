module RankSort (
  rankSort
) where

import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import           Genome

{-
go :: String
go = let pool = V.map toInd $ V.fromList [(4,4),(2,3),(4,3),(1,1),(1,3),(2,1),(3,1),(1,1)]
     in show $ rankSort $ sortPool pool (indCmpFit 0)
-}


{- |From a pool sorted by ascending fitness, add individuals one by one to
the best front where it is not dominated. If the individual is dominated by all
the current fronts, then add a new one. -}
rankSort :: Pool g -> Fronts g
rankSort =
  V.foldl insert_ind (V.fromList [V.fromList []])
    where
  insert_ind :: Fronts g -> Ind g -> Fronts g
  insert_ind fronts ind = if ind `domByFront` V.last fronts
    then V.snoc fronts (V.fromList [ind { rank = V.length fronts }]) --new front
    else V.update fronts (V.fromList [(ind_rank, upd_front)]) --add to existing
      where
    ind_rank = binSearch ind fronts
    upd_front = V.snoc (fronts V.! ind_rank) ind { rank = ind_rank }


{- | Find the lowest index of the front where the individual can be inserted
without being dominated using binary search. The correct insertion point for an
individual is k if if it's dominated by front (k-1) but not by front k.
Since the each front is already sorted by fitness, it is only necessary to check
whether or not the last individual in a front dominates the given individual.-}
binSearch :: Ind g -> Fronts g -> Int
binSearch ind_a fronts = binSearch' 0 (V.length fronts - 1)
      where
  binSearch' :: Int -> Int -> Int
  binSearch' low high
    | low == high = low
    | ind_b `dominates` ind_a = binSearch' (mid+1) high
    | otherwise = binSearch' low mid
      where
    ind_b = V.last (fronts V.! mid)
    mid = (low+high) `div` 2


{- | Returns True if individual x dominates individual y; False if y dominates x
or neither dominate each other. An individual dominates another if it is
no worse in all fitness objectives and strictly better in at least one objective.
For cost and distance bi-objective TSP, better means less. -}
dominates :: Ind g    -- ^ 'individual x'
          -> Ind g   -- ^ 'individual y'
          -> Bool
ind_a `dominates` ind_b = strictly_better && no_worse
    where
  strictly_better = VG.or $ VG.zipWith (<) x y
  no_worse = VG.and $ VG.zipWith (<=) x y
  x = fitnesses ind_a
  y = fitnesses ind_b


-- | Returns whether or not the individual is dominated by any members of the front
domByFront :: Ind g -> Pool g -> Bool
ind `domByFront` front = V.any (`dominates` ind) front
