module GeneticOps (
  displaceMutation, orderedCrossover
) where

import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Unboxed as VU
import           System.Random

import           RandUtils


go = do
  g <- newStdGen
  let genome_a = VU.fromList [1,4,5,0,2,7,3,9,6,8] :: VU.Vector Int
      genome_b = VU.fromList [8,4,3,2,9,1,7,0,5,6] :: VU.Vector Int
  print $ orderedCrossover genome_a genome_b g


displaceMutation :: (VG.Vector v a, RandomGen g) => v a -> g -> (v a, g)
displaceMutation genome rgen =
  (mutated, rgen2)
    where
  (i_left, i_right, rgen1) = randIndices (VG.length genome) rgen
  n = i_right - i_left
  part = VG.slice i_left n genome
  leftovers = VG.take i_left genome VG.++ VG.drop i_right genome
  (insert_pos, _, rgen2) = randIndices (VG.length leftovers) rgen1
  mutated = VG.take insert_pos leftovers VG.++ part VG.++ VG.drop insert_pos leftovers


{-
if np.random.rand() < mutation_rate:
    i_left, i_right = _random_indices(len(genome))
    part = genome[i_left:i_right]
    mutated_genome = np.concatenate([genome[:i_left], genome[i_right:]])
    insert_pos = np.random.randint(0, len(mutated_genome)+1)
    mutated_genome = np.insert(mutated_genome, insert_pos, part)
    return mutated_genome
return genome
-}

orderedCrossover :: (VG.Vector v a, Eq a, RandomGen g) => v a -> v a -> g -> (v a, v a, g)
orderedCrossover parent_a parent_b rgen =
  (child_a, child_b, rgen2)
    where
  (p_left, p_right, rgen1) = randIndices (VG.length parent_a) rgen
  n = p_right - p_left
  c_a_mid = VG.slice p_left n parent_a
  c_b_mid = VG.slice p_left n parent_b
  c_a_elems = VG.filter (`VG.notElem` c_a_mid) parent_b
  c_b_elems = VG.filter (`VG.notElem` c_b_mid) parent_a
  (r_left, r_right, rgen2) = randIndices (VG.length parent_a) rgen1
  child_a = VG.take r_left c_a_elems VG.++ c_a_mid VG.++ VG.drop r_right c_a_elems
  child_b = VG.take r_left c_b_elems VG.++ c_b_mid VG.++ VG.drop r_right c_b_elems

{-
child_a[i_left:i_right] = genome_a[i_left:i_right]
child_b[i_left:i_right] = genome_b[i_left:i_right]
child_chain = np.concatenate((np.arange(i_right, size), np.arange(0, i_left)))
parent_chain = np.concatenate((np.arange(i_right, size), np.arange(0, i_right)))
for chain_i, i in enumerate(child_chain):
    for j in parent_chain[chain_i:]:
        if not np.any(genome_b[j] == child_a):
            child_a[i] = genome_b[j]
            break
    for j in parent_chain[chain_i:]:
        if not np.any(genome_a[j] == child_b):
            child_b[i] = genome_a[j]
            break
-}
