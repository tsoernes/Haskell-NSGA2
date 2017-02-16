-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  None
-- License     :  MIT
--
-- Maintainer  : Torstein Sørnes
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
  ds <- loadDS
  let eap = eaPreset1
  (c, a) <- eaRunner ds eap
  print $ showFits c
  print $ showFits a
  print $ showRanks c
  print $ showRanks a


eaPreset1 :: EAProblem
eaPreset1 = EAProblem
  { nGenerations = 100
  , popSize      = 100
  , nCities      = 48
  , tournSize     = 2
  , mutationRate  = 0.05
  , crossoverRate = 0.80
  }
