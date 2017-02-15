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

 {-
--eaRunner' :: (MonadRandom m) => DataSet -> EAProblem -> m (Pool, Pool)
eaRunner' :: DataSet -> EAProblem -> (Pool, Pool)
eaRunner ds eap =
  runGen (children, V.empty) (nGenerations eap)
    where
  (children) = newPool (popSize eap) (nCities eap)
  --runGen :: (MonadRandom m) => (Pool, Pool) -> Int -> m (Pool, Pool)
  runGen :: (Pool, Pool) -> Int -> (Pool, Pool)
  runGen res 0 = res
  runGen (children', adults) gen =
    runGen (newChildren, newAdults) (gen-1)
      where
    childrenE = evalFitnesses children' ds
    newAdults = adultSelectRankCdist childrenE adults
    (newParents) = tournamentSelect newAdults (tournSize eap)
    (newChildren) = reproduce newParents eap
-}

-- Evaluate fitnesses ->
-- Select adults ->
-- Select parents ->
-- Reproduce ->
-- Repeat
eaRunner :: (MonadRandom m) => DataSet -> EAProblem -> m (Pool, Pool)
eaRunner ds eap = do
  initPool <- newPool (popSize eap) (nCities eap)
  -- | Run through one generation, generating/selecting new children and new adults
  let runGen :: (MonadRandom m) => (Pool, Pool) -> Int -> m (Pool, Pool)
      runGen (children, adults) _ = do
        let children' = evalFitnesses children ds
            newAdults = adultSelectRankCdist children' adults
        newParents <- tournamentSelect newAdults (tournSize eap)
        newChildren <- reproduce newParents eap
        return (newChildren, newAdults)

  return $ foldM runGen (initPool, V.empty) [0..(nGenerations eap)]
{-
reproduce' :: (MonadRandom m) => Pool -> EAProblem -> m Pool
reproduce' parents eap =
  V.foldl mate V.empty $ V.zip (V.take half parents) (V.drop half parents)
    where
  half = V.length parents `div` 2
  mate :: (MonadRandom m) => (Ind, Ind) -> m Pool
  mate (a, b) =
      where
    [c_chance, m1_chance, m2_chance] <- take 3 getRandoms
    (ac, bc) = if c_chance < crossoverRate eap
      then orderedCrossover a b
      else (a, b)
    am = if m1_chance < mutationRate eap
      then displaceMutation ac
      else ac
    bm = if m2_chance < mutationRate eap
      then displaceMutation bc
      else bc
    cs = V.fromList [am, bm]
-}

reproduce :: (MonadRandom m) => Pool -> EAProblem -> m Pool
reproduce parents eap = vectorConcatMapM mate mates
    where
  half = V.length parents `div` 2
  mates = V.zip (V.take half parents) (V.drop half parents)
  mate :: (MonadRandom m) => (Ind, Ind) -> m Pool
  mate (a, b) = do
    let p1g = genome a
        p2g = genome b
    r1 <- getRandomR(0,5)
    r2 <- getRandomR(0,5)
    return $ V.fromList [a { genome = p1g }, a { genome = p2g }]


vectorConcatMapM :: Monad m => (a -> m (V.Vector b)) -> V.Vector a -> m (V.Vector b)
vectorConcatMapM f v = join <$> sequence (fmap f v)


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
