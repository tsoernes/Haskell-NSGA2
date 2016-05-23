module ParentSelection (
  tournamentSelect
) where

import qualified Data.Vector   as V
import           Prelude       as P
import           System.Random

import           Genome
import           RandUtils
import           SortUtils     (indCmpCrowded)


-- No replacement
tournamentSelect :: (RandomGen g) => Pool -> Int -> g-> (Pool, g)
tournamentSelect adults k g =
  V.foldl step (V.empty, g) $ V.enumFromN 0 (V.length adults)
    where
  step :: (RandomGen g) => (Pool, g) -> Int -> (Pool, g)
  step (parents, g1) _ =
    (V.snoc parents winner, g2)
      where
    (group, g2) = shuffle adults k g1
    winner = V.maximumBy indCmpCrowded group
