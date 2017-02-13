module Population (
  eaRunner,
  EAProblem(..)
) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           System.Random

import           AdultSelection
import           GeneticOps
import           Genome
import           Load
import           ParentSelection


data EAProblem = EAProblem
    { nGenerations  :: Int
    , popSize       :: Int
    , nCities       :: Int
    , tournSize     :: Int
    , mutationRate  :: Float
    , crossoverRate :: Float
    }


eaRunner :: (RandomGen g) => DataSet -> EAProblem -> g -> (Pool, Pool, g)
eaRunner ds eap g =
  runGen (children, V.empty, g1) (nGenerations eap)
    where
  (children, g1) = newPool (popSize eap) (nCities eap) g
  runGen :: (RandomGen g) => (Pool, Pool, g) -> Int -> (Pool, Pool, g)
  runGen res 0 = res
  runGen (children', adults, g2) gen =
    runGen (newChildren, newAdults, g4) (gen-1)
      where
    childrenE = evalFitnesses children' ds
    newAdults = adultSelectRankCdist childrenE adults
    (newParents, g3) = tournamentSelect newAdults (tournSize eap) g2
    (newChildren, g4) = reproduce newParents eap g3


reproduce :: (RandomGen g) => Pool -> EAProblem -> g -> (Pool, g)
reproduce parents eap g =
  V.foldl mate (V.empty, g) $ V.zip (V.take half parents) (V.drop half parents)
    where
  half = V.length parents `div` 2
  mate :: (RandomGen g) => (Pool, g) -> (Ind, Ind) -> (Pool, g)
  mate (children, g1) (a, b) =
    (children V.++ cs, g7)
      where
    (c_chance, g2) = randomR (0::Float,1) g1
    (m1_chance, g3) = randomR (0::Float,1) g2
    (m2_chance, g4) = randomR (0::Float,1) g3
    (ac, bc, g5) = if c_chance < crossoverRate eap
      then orderedCrossover a b g4
      else (a, b, g4)
    (am, g6) = if m1_chance < mutationRate eap
      then displaceMutation ac g5
      else (ac, g5)
    (bm, g7) = if m2_chance < mutationRate eap
      then displaceMutation bc g6
      else (bc, g6)
    cs = V.fromList [am, bm]


evalFitnesses :: Pool -> DataSet -> Pool
evalFitnesses children ds =
  V.map evalFit children
    where
  evalFit ind = ind { fitnesses = VU.fromList [totDist, totCost] }
      where
    neighbors = VU.zip (VU.init $ genome ind) (VU.tail $ genome ind)
    totDist = VU.foldr' d 0 neighbors
    totCost = VU.foldr' c 0 neighbors
    d (a,b) acc = acc + ((dist ds V.! a) V.! b)
    c (a,b) acc = acc + ((cost ds V.! a) V.! b)
