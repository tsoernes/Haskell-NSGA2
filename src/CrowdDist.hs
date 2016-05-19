module CrowdDist (
  cDistAssignFront
) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

import           Genome
import           SortUtils


-- Assign crowding distance to individuals in a non-dominated front
cDistAssignFront :: Pool -> Pool
cDistAssignFront front =
  V.foldl cDistAssignFit ndf0 fit_idxs
    where
  {- |Initialize all crowding distances to 0 so that they don't carry over
  from previous runs -}
  ndf0 = V.map (`setCdistI` 0) front
  fit_idxs = V.enumFromN 0 (VU.length $ fitnesses $ front V.! 0) -- n. fit. objectives


  -- Assign crowding distance to individuals for a given fitness
cDistAssignFit :: Pool -> Int -> Pool
cDistAssignFit front fit_idx =
  V.foldl cdassign start_front ind_idxs
    where
  cdassign = cDistAssignInd fit_idx fit_factor
  fit_factor = getFit start_front l fit_idx - getFit start_front 0 fit_idx
  {- |The first and last individual in the sorted front should have infinite
  crowding distance -}
  start_front = setCdistP l (1/0) $ setCdistP 0 (1/0) sorted
  l = V.length front-1
  sorted = sortPool front (indCmpFit fit_idx)
  ind_idxs = V.enumFromN 1 (V.length front - 2)


-- Assign crowding distance to an individual for a given fitness
cDistAssignInd :: Int -> Float -> Pool -> Int -> Pool
cDistAssignInd fit_idx fit_factor front ind_idx =
  new_front
    where
  new_front = setCdistP ind_idx cdist' front
  {- |The distance is scaled by the difference between max and min fitness
  (fit_fac) in the front and then summed up for all fitnesses -}
  cdist' = cdist (front V.! ind_idx) + (next - prev) / fit_factor
  next = getFit front (ind_idx+1) fit_idx
  prev = getFit front (ind_idx-1) fit_idx
