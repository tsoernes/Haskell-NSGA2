module ParentSelection (
  tournamentSelect
) where

import qualified Data.Vector         as V
import           Data.Vector.Unboxed as VU
import           Prelude             as P
import           System.Random

import           CrowdDist
import           Genome
import           RandUtils
import           RankSort
import           SortUtils


-- No replacement
tournamentSelect :: (RandomGen g) => Pool -> Int -> g-> (Pool, g)
tournamentSelect adults k g =
  V.foldl step (V.empty, g) steps
    where
  steps = V.enumFromN 0 (V.length adults)
  step :: (RandomGen g) => (Pool, g) -> Int -> (Pool, g)
  step (parents, g1) _ =
    (V.snoc parents winner, g2)
      where
    (group, g2) = shuffle adults k g1
    winner = V.maximumBy indCmpCrowded group
