module Population (
  eaRunner,
  EAProblem(..)
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


data EAProblem = EAProblem
    { nGenerations  :: Int
    , popSize       :: Int
    , nCities       :: Int
    , tournSize     :: Int
    , mutationRate  :: Float
    , crossoverRate :: Float
    }


-- Evaluate fitnesses ->
-- Select adults ->
-- Select parents ->
-- Reproduce ->
-- Repeat
eaRunner :: (MonadRandom m) => DataSet -> EAProblem -> m (Pool, Pool)
eaRunner ds eap = do
  initPool <- newPool (popSize eap) (nCities eap)
  -- Run through one generation, generating/selecting new children and new adults
  let runGen :: (MonadRandom m) => (Pool, Pool) -> Int -> m (Pool, Pool)
      runGen (children, adults) _ = do
        let children' = evalFitnesses children ds
            newAdults = adultSelectRankCdist children' adults
        newParents <- tournamentSelect newAdults (tournSize eap)
        newChildren <- reproduce newParents eap
        return (newChildren, newAdults)

  foldM runGen (initPool, V.empty) [0..(nGenerations eap)]


reproduce :: (MonadRandom m) => Pool -> EAProblem -> m Pool
reproduce parents eap = vectorConcatMapM mate mates
    where
  half = V.length parents `div` 2
  mates = V.zip (V.take half parents) (V.drop half parents)
  mate :: (MonadRandom m) => (Ind, Ind) -> m Pool
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


vectorConcatMapM :: Monad m => (a -> m (V.Vector b)) -> V.Vector a -> m (V.Vector b)
vectorConcatMapM f v = join <$> sequence (fmap f v)


-- Can this be made more general? E.g. to work for any number of fitnesses.
-- then genome (and dataset?) need to be abstracted and be less descriptive and
-- intuitive?
evalFitnesses :: Pool -> DataSet -> Pool
evalFitnesses children dataset = V.map evalFit children
    where
  evalFit ind = ind { fitnesses = VU.fromList [totalDist, totalCost] }
      where
    neighbors = VU.zip (VU.init $ genome ind) (VU.tail $ genome ind)
    totalDist = VU.foldr' sumDist 0 neighbors
    totalCost = VU.foldr' sumCost 0 neighbors
    sumDist (a,b) acc = acc + ((dist dataset V.! a) V.! b)
    sumCost (a,b) acc = acc + ((cost dataset V.! a) V.! b)
