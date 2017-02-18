module Population (
  eaRunner,
  EAProblem(..),
  evalFitnessesTSP
) where

import           AdultSelection
import           Control.Monad
import           Control.Monad.Random
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           GeneticOps
import           Genome
import           Load
import           ParentSelection


-- @TODO should mutation and crossover function be abstracted out into EAProblem?
data EAProblem = EAProblem
    { nGenerations      :: Int -- ^ Number of generations to run
    , popSize           :: Int -- ^ Population size
    , nCities           :: Int
    , tournSize         :: Int
    , mutationRate      :: Double
    , crossoverRate     :: Double
    , strictlyBetterCmp :: V.Vector (Fitness -> Fitness -> Bool) -- ^ Comparator for each fitness to determine if a fitness value is strictly better than another. E.g. (<)
    , noWorseCmp        :: V.Vector (Fitness -> Fitness -> Bool) -- ^ Comparator for each fitness to determine if a fitness value is no worse than another. E.g. (<=)
    }


-- Evaluate fitnesses ->
-- Select adults ->
-- Select parents ->
-- Reproduce ->
-- Repeat
eaRunner :: (MonadRandom m) => DataSet -> EAProblem -> m (Pool Int, Pool Int)
eaRunner ds eap = do
  initPool <- newTSPPool (popSize eap) (nCities eap)
  -- Run through one generation, generating/selecting new children and new adults
  let --runGen :: (MonadRandom m, Eq g) => (Pool g, Pool g) -> Int -> m (Pool g, Pool g)
      runGen (children, adults) _ = do
        let children' = evalFitnessesTSP children ds
            newAdults = adultSelectRankCdist children' adults
        newParents <- tournamentSelect newAdults (tournSize eap)
        newChildren <- reproduce newParents eap
        return (newChildren, newAdults)

  foldM runGen (initPool, V.empty) [0..(nGenerations eap)]


-- | Generate children from a pool of 'adults' using crossover and mutation.
reproduce :: (MonadRandom m, Eq g, VU.Unbox g) => Pool g -> EAProblem -> m (Pool g)
reproduce parents eap = vectorConcatMapM mate mates
    where
  half = V.length parents `div` 2
  mates = V.zip (V.take half parents) (V.drop half parents)
  mate :: (MonadRandom m, Eq g, VU.Unbox g) => (Ind g, Ind g) -> m (Pool g)
  mate (ind_a, ind_b) = do
    let a = genome ind_a
        b = genome ind_b
    [c_chance, m1_chance, m2_chance] <- take 3 `fmap` getRandoms
    (a_cross, b_cross) <- if c_chance < crossoverRate eap
                          then orderedCrossover a b
                          else return (b, a)
    a_mut <- if m1_chance < mutationRate eap
              then displaceMutation a_cross
              else return a_cross
    b_mut <- if m2_chance < mutationRate eap
              then displaceMutation b_cross
              else return b_cross
    return $ V.fromList [ind_a { genome = a_mut }, ind_b { genome = b_mut }]


-- Can this be made more general? E.g. to work for any number of fitnesses.
-- then genome (and dataset?) need to be abstracted and be less descriptive and
-- intuitive?
-- | Calculate the total cost and total distance of the route
-- which the genome represents
evalFitnessesTSP :: Pool Int -> DataSet -> Pool Int
evalFitnessesTSP children dataset = V.map evalFit children
    where
  evalFit ind = ind { fitnesses = VU.fromList [totalDist, totalCost] }
      where
    neighbors = VU.zip (VU.init $ genome ind) (VU.tail $ genome ind)
    totalDist = VU.foldr' sumDist 0 neighbors
    totalCost = VU.foldr' sumCost 0 neighbors
    sumDist (a,b) acc = acc + ((dist dataset V.! a) V.! b)
    sumCost (a,b) acc = acc + ((cost dataset V.! a) V.! b)

vectorConcatMapM :: Monad m => (a -> m (V.Vector b)) -> V.Vector a -> m (V.Vector b)
vectorConcatMapM f v = join <$> sequence (fmap f v)
