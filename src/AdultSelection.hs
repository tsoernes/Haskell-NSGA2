module AdultSelection (
  aduSelectRankCdist
) where

import qualified Data.Vector.Generic as V

import           CrowdDist
import           Genome
import           RankSort
import           SortUtils


{- |Select adults from a combined pool of the children and adults from the
previous generation based on rank and crowding distance.-}
aduSelectRankCdist :: Pool -> Pool -> Pool
aduSelectRankCdist children adults =
  rank_pool V.++ crowd_pool
    where
  fronts = rankSort (children V.++ adults)
  n = V.length children
  acc_len = V.postscanl (\i li -> i + V.length li) 0 fronts
  -- | n_complete is the number of fronts that can be completely fitted into the pool.
  n_complete = V.length $ V.takeWhile (<=n) acc_len
  {- |The rank pool contains the individuals from the lowest fronts that can be
  completely fitted into the adult pool. -}
  complete_fronts = V.take n_complete fronts
  rank_pool = V.foldl (V.++) V.empty $ V.map cDistAssignFront complete_fronts
  last_front = cDistAssignFront $ fronts V.! n_complete
  {- |The crowd pool contains the individuals from the front with the lowest
  rank that cannot be completely fitted into the adult pool. The individuals
  with the highest crowding distance from that front are chosen.
  -}
  n_ind_remaining = n - V.length rank_pool
  crowd_pool = V.take n_ind_remaining (sortPool last_front indCmpReverseDist)
