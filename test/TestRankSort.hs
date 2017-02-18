module TestRankSort (
  tests
) where

import           Control.Monad.ST
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           GeneticOps
import           Genome
import           Load
import           Population
import           SortUtils
import           Test.Tasty
import           Test.Tasty.HUnit

tests = testGroup "RankSort" [unitTests]

unitTests = testGroup "Unit tests"
  [
  ]
