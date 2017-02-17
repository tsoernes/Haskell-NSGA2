module Genome where

import           Control.Monad.Random
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           RandUtils

data Ind = Ind
    { fitnesses :: VU.Vector Double
    , genome    :: VU.Vector Int
    , rank      :: Int -- ^ Equals the index of the non-dominated (pareto) front, i.e. lower is better
    , cdist     :: Double -- ^ Crowding distance
    } deriving (Eq)

defaultFits :: VU.Vector Double
defaultFits = VU.fromList [-1.0, -1.0]
defaultRank :: Int
defaultRank = -1
defaultCdist :: Double
defaultCdist = -1.0


type Pool =
  V.Vector Ind

type Fronts =
  V.Vector Pool


instance Show Ind where
   show Ind{ fitnesses = f
           , rank = r
           , genome = g
           } = "f" ++ show f ++ "r" ++ show r ++ "g" ++ show g ++ " "


test :: IO()
test = do
  p <- evalRandIO (newPool 5 10)
  print $ showGenomes p


showFits :: Pool -> String
showFits = V.foldl1 (++) . V.map (show . fitnesses)


showRanks :: Pool -> String
showRanks = V.foldl1 (++) . V.map (show . rank)


showGenomes :: Pool -> String
showGenomes = V.foldl1 (++) . V.map (show . genome)


-- @TODO this should probably not be in this module
-- | Create a new pool of individuals
newPool :: (MonadRandom m)
        => Int      -- ^ Number of individuals
        -> Int      -- ^ Genome length
        -> m Pool
newPool nInds genomeLen = V.replicateM nInds (newInd genomeLen)


-- | Create a new individual with a random genome of length 'genomeLen' and
-- remaining values set to defaults
newInd :: (MonadRandom m) => Int -> m Ind
newInd genomeLen = do
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
        -> Pool
        -> Pool -- ^ the pool with the updated individual
setRank idx rank' pool = V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { rank = rank' }


setCdist :: Int    -- ^ index of individual
          -> Double -- ^ crowding distance
          -> Pool
          -> Pool  -- ^ the pool with the updated individual
setCdist idx cdist' pool = V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { cdist = cdist' }


getFit :: Pool
        -> Int -- ^ index of individual
        -> Int -- ^ fitness index
        -> Double -- ^ fitness
getFit pool indIdx fitIdx = fitnesses (pool V.! indIdx) VU.! fitIdx
