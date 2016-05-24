module Genome (
  Ind(fitnesses, genome, rank, cdist), Pool, Fronts,
  newInd, setRank, setCdist, getFit
) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           System.Random

import           RandUtils


data Ind = Ind
    { fitnesses :: VU.Vector Float
    , genome    :: VU.Vector Int
    , rank      :: Int
    , cdist     :: Float
    }


type Pool =
  V.Vector Ind


type Fronts =
  V.Vector Pool


instance Show Ind where
   show Ind{ fitnesses = f, rank = r, genome = g } = show f ++ "r" ++ show r ++ show g


go :: IO()
go = do
  g <- newStdGen
  let (p, _) = newPool 5 10 g
  print $ showGenomes p


showFits :: Pool -> String
showFits =
  V.foldl1 (++) . V.map (show . fitnesses)


showRanks :: Pool -> String
showRanks =
  V.foldl1 (++) . V.map (show . rank)


showGenomes :: Pool -> String
showGenomes =
  V.foldl1 (++) . V.map (show . genome)


-- Create a new pool with @n_inds@ individual with random genomes of length
-- @l_genome@
newPool :: (RandomGen g) => Int -> Int -> g -> (Pool, g)
newPool n_inds l_genome g =
  V.foldl add (V.empty, g) (V.enumFromN 0 n_inds)
    where
  add :: (RandomGen g) => (Pool, g) -> Int -> (Pool, g)
  add (pool, g') _ =
    (V.snoc pool ind, g'')
      where
    (ind, g'') = newInd l_genome g'


-- Create a new individual with a random genome of length @l_genome@
newInd :: (RandomGen g) => Int -> g -> (Ind, g)
newInd l_genome g =
  (Ind (VU.fromList [-1, -1]) genome' (-1) (-1), g')
    where
  (genome', g') = randVector l_genome g


  -- Set rank of an individual and return the pool with the updated
  -- individual
setRank :: Int -> Int -> Pool -> Pool
setRank idx rank' pool =
  V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { rank = rank' }


-- Set crowding distance of an individual and return the pool with the updated
-- individual
setCdist :: Int -> Float -> Pool -> Pool
setCdist idx cdist' pool =
  V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { cdist = cdist' }


getFit :: Pool -> Int -> Int -> Float
getFit pool indIdx fitIdx =
  fitnesses (pool V.! indIdx) VU.! fitIdx
