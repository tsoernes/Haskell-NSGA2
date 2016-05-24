module Population (

) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           System.Random

import           Genome
import           Load
import           SortUtils


data EAProblem = EAProblem
    { nGenerations  :: Int
    , popSize       :: Int
    , nCities       :: Int
    , tournSize     :: Int
    , mutationRate  :: Float
    , crossoverRate :: Float}


eaRunner :: (RandomGen g) => DataSet -> EAProblem -> g -> Pool
eaRunner ds eap g = undefined
{-
while population.generation < problem.generation_limit:
  population.generation += 1
  population.evaluate_fitnesses()  # Calculate total cost and total distance for each route/individual
  population.select_adults()
  population.select_parents()
  population.reproduce()
  if population.generation % (problem.generation_limit / 5) == 0:
      logging.info("\t\t Generation %s/%s", population.generation, problem.generation_limit)
      fronts.append(population.get_front(0))
-}


reproduce :: (RandomGen g) => Pool -> EAProblem -> g -> Pool
reproduce children eap g = undefined
{-
def reproduce(self):
        """
        Generate children from the selected parents by first crossing genes then mutating
        """
        # An individual can reproduce with itself. Probably not optimal.
        self.children = \
            [self.genome(self.n_cities, genotype=self.mutate(child_genome, self.problem.mutation_rate))
                for parent_a, parent_b in zip(islice(self.parents, 0, None, 2), islice(self.parents, 1, None, 2))
                for child_genome in self.crossover(parent_a.genotype, parent_b.genotype, self.crossover_rate)]
-}


evalFitnesses :: Pool -> DataSet -> Pool
evalFitnesses children ds =
  V.map evalFit children
    where
  evalFit ind = ind { fitnesses = VU.fromList [totDist, totCost] }
      where
    totDist = VU.foldl d 0 (VU.tail $ genome ind)
    totCost = VU.foldl c 0 (VU.tail $ genome ind)
    d acc idx = acc + ((dist ds V.! (idx-1)) V.! idx)
    c acc idx = acc + ((cost ds V.! (idx-1)) V.! idx)
