module TestCrowdDist (
) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

import           Genome
import           SortUtils

cDistAssignFront :: Pool -> Pool

cDistAssignFit :: Pool -> Int -> Pool

cDistAssignInd :: Int -> Float -> Pool -> Int -> Pool
