module TestGeneticOps (
  tests
) where

import qualified Data.Vector.Unboxed as VU
import           GeneticOps
import           Test.Tasty
import           Test.Tasty.HUnit

tests = testGroup "GeneticOps" [unitTests]

parent_a_1 = VU.fromList [1::Int, 4, 2, 8, 5, 7, 3, 6, 9]
parent_b_1 = VU.fromList [7::Int, 5, 3, 1, 9, 8, 6, 4, 2]
cross_idxs_1 = (3, 7)
crossed_child_a_1 = VU.fromList [1::Int, 9, 6, 8, 5, 7, 3, 4, 2]
crossed_child_b_1 = VU.fromList [5::Int, 7, 3, 1, 9, 8, 6, 4, 2]
dispmut_slice_1 = (3, 7)
dispmut_in_1 = 1
dispmut_parent_a_1 = VU.fromList [1::Int, 8, 5, 7, 3, 4, 2, 6, 9]

parent_a_2 = VU.fromList [1::Int, 2, 3, 4, 5, 6, 7, 8, 9]
parent_b_2 = VU.fromList [4::Int, 5, 2, 1, 8, 7, 6, 9, 3]
crossed_child_a_2 = VU.fromList [2::Int, 1, 8, 4, 5, 6, 7, 9, 3]
crossed_child_b_2 = VU.fromList [3::Int, 4, 5, 1, 8, 7, 6, 9, 2]
cross_idxs_2 = (3, 7)

unitTests = testGroup "Unit tests"
  [ testCase "Ordered Crossover test 1" $
      assertEqual "" (crossed_child_a_1, crossed_child_b_1) (orderedCrossoverStat cross_idxs_1 parent_a_1 parent_b_1)
  , testCase "Ordered Crossover test 2" $
      assertEqual "" (crossed_child_a_2, crossed_child_b_2) (orderedCrossoverStat cross_idxs_2 parent_a_2 parent_b_2)
  , testCase "Displacement Mutation 1" $
      assertEqual "" (dispmut_parent_a_1) (displaceMutationStat dispmut_slice_1 dispmut_in_1 parent_a_1)
  ]
