module TestLoad (
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

test :: IO()
test = do
  ds <- loadDS
  V.mapM_ print $ cost ds

tests = testGroup "RankSort" [unitTests]

unitTests = testGroup "Unit tests"
  [
  ]
