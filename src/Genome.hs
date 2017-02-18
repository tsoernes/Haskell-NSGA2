module Genome where

import           Control.Monad.Random
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           RandUtils

data Ind g = Ind
    { fitnesses :: VU.Vector Fitness
    , genome    :: VU.Vector g
    , rank      :: Int -- ^ Equals the index of the non-dominated (pareto) front, i.e. lower is better
    , cdist     :: Float -- ^ Crowding distance
    } deriving (Eq)

type Fitness = Float



defaultFits :: VU.Vector Float
defaultFits = VU.fromList [-1.0, -1.0]
defaultRank :: Int
defaultRank = -1
defaultCdist :: Float
defaultCdist = -1.0


type Pool g = V.Vector (Ind g)

type Fronts g = V.Vector (Pool g)


instance Show (Ind g) where
   show Ind{ fitnesses = f
           , rank = r
           } = "f" ++ show f ++ "r" ++ show r ++ " "


showFits :: Pool g -> String
showFits = V.foldl1 (++) . V.map (show . fitnesses)


showRanks :: Pool g -> String
showRanks = V.foldl1 (++) . V.map (show . rank)


showGenomes :: (Show g, VU.Unbox g) => Pool g -> String
showGenomes = V.foldl1 (++) . V.map (show . genome)


-- @TODO this should probably not be in this module
-- | Create a new pool of individuals
newTSPPool :: (MonadRandom m)
        => Int      -- ^ Number of individuals
        -> Int      -- ^ Genome length
        -> m (Pool Int)
newTSPPool nInds genomeLen = V.replicateM nInds (newTSPInd genomeLen)


-- | Create a new individual with a random genome of length 'genomeLen' and
-- remaining values set to defaults
newTSPInd :: (MonadRandom m) => Int -> m (Ind Int)
newTSPInd genomeLen = do
  genome' <- randVector genomeLen
  return (Ind defaultFits genome' defaultRank defaultCdist)


{-
setRank :: (MonadRandom m) => Int -> Int -> m Pool -> m Pool
setRank idx rank' pool = do
  p <- pool
  let updated_ind = (p V.! idx) { rank = rank' }
      np = V.update p (V.fromList [(idx, updated_ind)])
  return np
-}


setRank :: Int  -- ^ index of individual
        -> Int  -- ^ rank
        -> Pool g
        -> Pool g -- ^ the pool with the updated individual
setRank idx rank' pool = V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { rank = rank' }


setCdist :: Int    -- ^ index of individual
          -> Float -- ^ crowding distance
          -> Pool g
          -> Pool g  -- ^ the pool with the updated individual
setCdist idx cdist' pool = V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { cdist = cdist' }


getFit :: Pool g
        -> Int -- ^ index of individual
        -> Int -- ^ fitness index
        -> Float -- ^ fitness
getFit pool indIdx fitIdx = fitnesses (pool V.! indIdx) VU.! fitIdx
