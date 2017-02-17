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

pool1 :: IO Pool
pool1 = do
  pool <- newPool 10 10
  ds <- loadDS
  return $ evalFitnesses pool ds

unitTests = testGroup "Unit tests"
  [ testCase "Sort Pool" $ do
      pool <- pool1
      let cmp = indCmpFit 0
      assertEqual "Sort once equals sort twice"  (sortPool pool cmp) (sortPool (sortPool pool cmp) cmp)
  ]
