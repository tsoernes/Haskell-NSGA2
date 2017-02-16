module Main (
    main
) where

import           Control.Monad
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestGeneticOps   as GO

main :: IO()
main = defaultMain GO.tests
