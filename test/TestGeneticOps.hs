module TestGeneticOps (
) where

import qualified Data.Vector.Generic as VG
import           System.Random

import           Genome
import           RandUtils


displaceMutation :: (RandomGen g) => Ind -> g -> (Ind, g)
displaceMutation = undefined

orderedCrossover :: (RandomGen g) => Ind -> Ind -> g -> (Ind, Ind, g)
orderedCrossover = undefined
