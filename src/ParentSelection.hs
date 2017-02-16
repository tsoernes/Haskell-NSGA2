module ParentSelection (
  tournamentSelect
) where


import           Control.Monad.Random
import qualified Data.Vector.Generic  as V
import           Prelude              as P

import           Genome
import           RandUtils
import           SortUtils            (indCmpCrowded)


-- | Tournament Selection without replacement, i.e. an individual can be picked multiple times
tournamentSelect :: (MonadRandom m)
                 => Pool
                 -> Int -- ^ Tournament size 'k'
                 -> m Pool
tournamentSelect adults k = V.replicateM (V.length adults) pick
    where
  pick :: (MonadRandom m) => m Ind
  pick = do
    group <- randSample adults k
    return $ V.maximumBy indCmpCrowded group
