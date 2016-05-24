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


import           System.Random

import           Genome
import           Load
import           Population

main :: IO ()
main = do
  g <- getStdGen
  let ds = load1
      eap = eaPreset1
  eaRunner ds eap g


eaPreset1 :: EAProblem
eaPreset1 = EAProblem
  { nGenerations = 100
  , popSize      = 100
  , nCities      = 48
  , tournSize     = 2
  , mutationRate  = 0.05
  , crossoverRate = 0.80
}
