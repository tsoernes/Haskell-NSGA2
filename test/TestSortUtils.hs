module TestSortUtils (
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


tests = testGroup "SortUtils" [unitTests]

pool1 :: IO (Pool Int)
pool1 = do
  pool <- newTSPPool 10 10
  ds <- loadDS
  return $ evalFitnessesTSP pool ds

unitTests = testGroup "Unit tests"
  [ testCase "Sort Pool" $ do
      pool <- pool1
      let cmp = indCmpFit 0
      assertEqual "Sort once equals sort twice"  (sortPool pool cmp) (sortPool (sortPool pool cmp) cmp)
      -- May fail in very rare cases if the randomly generated pool is already sorted
      assertBool "Sort once does not equal unsorted" ((sortPool pool cmp) /= pool)
  ]
