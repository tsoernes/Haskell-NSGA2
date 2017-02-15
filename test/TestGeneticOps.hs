module TestGeneticOps (
) where

import qualified Data.Vector.Unboxed as VU
import           GeneticOps
import           Test.HUnit

tests = [TestLabel "orderedCrossover" testOrderedCrossover]

parent_a = VU.fromList [1::Int, 4, 2, 8, 5, 7, 3, 6, 9]
parent_b = VU.fromList [7::Int, 5, 3, 1, 9, 8, 6, 4, 2]

crossed_child_a = VU.fromList [1::Int, 9, 6, 8, 5, 7, 3, 4, 2]
crossed_child_b = VU.fromList [5::Int, 7, 3, 1, 9, 8, 6, 4, 2]
idxs = (3, 7)

testOrderedCrossover = TestCase(assertEqual "testOrderedCrossover" (orderedCrossoverStat idxs parent_a parent_b) (crossed_child_a, crossed_child_b))
