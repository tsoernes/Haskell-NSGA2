module RankSort (
  rankSort
) where

import qualified Data.Vector         as V
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
rankSort :: Pool -> Fronts
rankSort =
  V.foldl insert_ind (V.fromList [V.fromList []])
    where
  insert_ind :: Fronts -> Ind -> Fronts
  insert_ind fronts ind = if rank' == V.length fronts
    then V.snoc fronts (V.fromList [upd_ind])
    else V.update fronts (V.fromList [(rank', upd_front)])
      where
    upd_ind = rankIndex ind fronts
    rank' = rank upd_ind
    upd_front = V.snoc (fronts V.! rank') upd_ind


{- |Take an individual and find the index of the front where it is not
dominated by any members. This index is the rank of the individual.
Return the individual with its rank field set. -}
rankIndex :: Ind -> Fronts -> Ind
rankIndex ind fronts
  | ind `domByFront` V.last fronts = ind { rank = V.length fronts }
  | otherwise = ind { rank = binSearch ind fronts 0 (V.length fronts - 1) }


{- |Find the lowest index of the front where the individual can be inserted
without being dominated using binary search. The correct insertion point for an
individual is k if if it's dominated by front (k-1) but not by front k.
Since the each front is already sorted by fitness, it is only necessary to check
whether or not the last individual in a front dominates the given individual.-}
binSearch :: Ind -> Fronts -> Int -> Int -> Int
binSearch ind_a fronts low high
  | low == high = low
  | V.last (fronts V.! mid) `dominates` ind_a = binSearch ind_a fronts (mid+1) high
  | otherwise = binSearch ind_a fronts low mid
    where
  mid = (low+high) `div` 2


{- |Returns True if individual x dominates individual y; False if y dominates x
or neither dominate each other. An individual dominates another if it is
no worse in all objectives and strictly better in at least one objective.
For cost and distance biobjective TSP, better means less. -}
dominates :: Ind -> Ind -> Bool
ind_a `dominates` ind_b =
  strictly_better && no_worse
    where
  strictly_better = VU.or (VU.zipWith (<) x y)
  no_worse = VU.and (VU.zipWith (<=) x y)
  x = fitnesses ind_a
  y = fitnesses ind_b


-- |Returns whether or not an individual is dominated by any members of the front
domByFront :: Ind -> Pool -> Bool
ind `domByFront` front =
  V.any (`dominates` ind) front
