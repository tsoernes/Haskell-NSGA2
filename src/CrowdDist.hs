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
  V.foldl cDistAssignFit front0cd fitIdxs
    where
  {- |Initialize all crowding distances to 0 so that they don't carry over
  from previous runs -}
  front0cd = V.map (\ind -> ind { cdist = 0 }) front
  -- A list of fitness indexes, e.g. [0,1,2] if there are 3 fitness objectives
  fitIdxs = V.enumFromN 0 (VU.length $ fitnesses $ front V.! 0)


-- Assign crowding distance to a front for a given fitness
cDistAssignFit :: Pool -> Int -> Pool
cDistAssignFit front fitIdx =
  V.foldl cdAssign startFront indIdxs
    where
  cdAssign = cDistAssignInd fitIdx fitFactor
  {- The fitness factor is the difference between maxaimum and mininum
  fitness of the given fitness index in the front -}
  fitFactor = getFit startFront lastIndIdx fitIdx - getFit startFront 0 fitIdx
  {- The first and last individual in the sorted front should by definition
  have infinite crowding distance -}
  startFront = setCdist lastIndIdx (1/0) $ setCdist 0 (1/0) sorted
  lastIndIdx = V.length front-1
  sorted = sortPool front (indCmpFit fitIdx)
  indIdxs = V.enumFromN 1 (lastIndIdx-1)


-- Assign crowding distance to an individual for a given fitness
cDistAssignInd :: Int -> Float -> Pool -> Int -> Pool
cDistAssignInd fitIdx fitFactor front indIdx =
  setCdist indIdx cdist' front
    where
  {- The crowding distance for the given fitness is scaled by fitFactor and
  then added to the previously calculated crowding distance for other fitnesses -}
  cdist' = cdist (front V.! indIdx) + (next - prev) / fitFactor
  next = getFit front (indIdx+1) fitIdx
  prev = getFit front (indIdx-1) fitIdx
