-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import           Control.Monad
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

main :: IO ()
main = defaultMainWithOpts
       tests
       mempty

tests = TestList [TestLabel "test1" t, TestLabel "test2" j]
tests2 = TestList [TestLabel "test1" i, TestLabel "test2" j]
t = TestCase (assertBool "mytest" True)
j = TestCase (assertBool "mytest2" True)
i = TestCase (assertBool "mytest3" True)
