module Main (
    main
) where

import           Control.Monad
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestGeneticOps   as GO
import           TestSortUtils    as SU

main :: IO()
main = defaultMain allTests

allTests = testGroup "All Tests" [GO.tests, SU.tests]
