module GeneticOps (
  displaceMutation, displaceMutationStat, orderedCrossover, orderedCrossoverStat
) where

import           Control.Monad.Random
import qualified Data.Vector.Generic  as V
import           RandUtils


-- | Displacement Mutation:
-- A continous part of random length is taken out from a random position
-- and reinserted at a random position

-- Is it safe to unsafeThaw the input indvidual and mutate it?
-- E.g. is input Ind. used after function call? If not, is there any
-- performance advantage to doing safeThaw (O(n)) -> mutate ->
-- unsafeFreeze(O(1)) inside runST?
-- thaw->freeze gives no improvement if only using V.++
-- can some operations, here or elsewhere, be parallellized?
displaceMutation :: (MonadRandom m, V.Vector v a) => v a -> m (v a)
displaceMutation genome = do
  let len = V.length genome
  idxs@(l, r) <- randIndices len
  insert_pos <- getRandomR (0, len-(r-l))
  return $ displaceMutationStat idxs insert_pos genome


-- | Displacement mutation with chosen 'slice indexes' and 'insert position'
displaceMutationStat :: (V.Vector v a) => (Int, Int) -> Int -> v a -> v a
displaceMutationStat (left, right) insert_pos genome = mutated
    where
  n = right - left
  part = V.slice left n genome -- O(1)
  leftovers = V.take left genome V.++ V.drop right genome -- O(m+n)
  mutated = V.take insert_pos leftovers V.++ part V.++ V.drop insert_pos leftovers -- O(m+n)


-- | Ordered crossover (OX-1) between two individuals
-- http://creationwiki.org/pool/images/thumb/d/dc/Ox.png/800px-Ox.png
orderedCrossover :: (Eq a, MonadRandom m, V.Vector v a) => v a -> v a -> m (v a, v a)
orderedCrossover parent_a parent_b = do
  idxs <- randIndices (V.length parent_a)
  return (orderedCrossoverStat idxs parent_a parent_b)


-- | Ordered crossover with chosen 'slice indexes'
orderedCrossoverStat :: (Eq a, V.Vector v a) => (Int, Int) -> v a -> v a -> (v a, v a)
orderedCrossoverStat (left, right) parent_a parent_b = (child_a, child_b)
    where
      n = right - left
      -- Initialize a child with a slice from its parent
      c_a_mid = V.slice left n parent_a
      c_b_mid = V.slice left n parent_b
      -- Find missing genes from the opposite parents, in the order as they
      -- appear starting from the right of the slice looping around to the left
      c_a_miss = V.filter (`V.notElem` c_a_mid) (V.drop right parent_b V.++ V.take right parent_b)
      c_b_miss = V.filter (`V.notElem` c_b_mid) (V.drop right parent_a V.++ V.take right parent_a)
      splitpos = (V.length parent_a) - right
      (ra, la) = V.splitAt splitpos c_a_miss
      (rb, lb) = V.splitAt splitpos c_b_miss
      child_a = la V.++ c_a_mid V.++ ra
      child_b = lb V.++ c_b_mid V.++ rb
