module Population (

) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

import           Genome
import           SortUtils


type DataSet =
  (VU.Vector Int, VU.Vector Int)


data EAProblem = EAProblem
    { nGenerations  :: Int
    , popSize       :: Int
    , nCities       :: Int
    , tournSize     :: Int
    , mutationRate  :: Int
    , crossoverRate :: Int}


eaRunner :: EAProblem -> DataSet -> Pool
eaRunner = undefined
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


reproduce :: Pool -> Pool
reproduce = undefined
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


evalFitness :: Ind -> DataSet -> Ind
evalFitness = undefined
{-
def evaluate_fitness(self, child):
        total_distance = 0
        total_cost = 0
        for i in range(self.n_cities-1):
            city_a = child.genotype[i]
            city_b = child.genotype[i+1]
            #  The cost of travelling from a to b is equal to the cost of travelling from b to a
            total_distance += self.distances[city_a][city_b]
            total_cost += self.costs[city_a][city_b]

        child.fitnesses[0] = total_distance
        child.fitnesses[1] = total_cost

-}
