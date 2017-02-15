module GeneticOps (
  displaceMutation, orderedCrossover, orderedCrossoverStat
) where

import           Control.Monad.Random
import qualified Data.Vector.Generic  as V
import           Control.Monad.ST
import           RandUtils


-- Displacement Mutation:
-- A continous part (in the case of TSP, a sub-route) is taken out
-- and reinserted at a random position


-- Is it safe to unsafeThaw the input indvidual and mutate it?
-- E.g. is input Ind. used after function call? If not, is there any
-- performance advantage to doing safeThaw (O(n)) -> mutate ->
-- unsafeFreeze(O(1)) inside runST?

-- thaw->freeze gives no improvement if only using V.++

-- can some operations, here or elsewhere, be parallellized?
displaceMutation :: (MonadRandom m, V.Vector v a) => v a -> m (v a)
displaceMutation  genome = do
  let len = V.length genome
  idxs@(l, r) <- randIndices len
  insert_pos <- getRandomR (0, len-(r-l))
  return $ displaceMutationStat idxs insert_pos genome


displaceMutationStat :: (V.Vector v a) => (Int, Int) -> Int -> v a -> v a
displaceMutationStat (left, right) insert_pos genome = mutated
    where
  n = right - left
  part = V.slice left n genome -- O(1)
  leftovers = V.take left genome V.++ V.drop right genome -- O(m+n)
  mutated = V.take insert_pos leftovers V.++ part V.++ V.drop insert_pos leftovers -- O(m+n)


-- | Ordered crossover ("OX-1") between two individuals
-- http://creationwiki.org/pool/images/thumb/d/dc/Ox.png/800px-Ox.png
orderedCrossover :: (MonadRandom m, V.Vector v a) => v a -> v a -> m (v a, v a)
orderedCrossover parent_a parent_b = do
  idxs <- randIndices (V.length parent_a)
  return (orderedCrossoverStat idxs parent_a parent_b)


orderedCrossoverStat :: (V.Vector v a) => (Int, Int) -> v a -> v a -> (v a, v a)
orderedCrossoverStat (left, right) parent_a parent_b = (child_a, child_b)
    where
      n = right - left
      c_a_mid = V.slice left n parent_a
      c_b_mid = V.slice left n parent_b
      child_a = V.take left parent_a V.++ c_a_mid V.++ V.drop right parent_a
      child_b = V.take left parent_b V.++ c_b_mid V.++ V.drop right parent_b
