module Genome where

import           Control.Monad.Random
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           RandUtils

data Ind = Ind
    { fitnesses :: VU.Vector Float
    , genome    :: VU.Vector Int
    , rank      :: Int
    , cdist     :: Float -- crowding distance
    }

defaultFits :: VU.Vector Float
defaultFits = VU.fromList [-1.0, -1.0]
defaultRank :: Int
defaultRank = -1
defaultCdist :: Float
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
showFits =
  V.foldl1 (++) . V.map (show . fitnesses)


showRanks :: Pool -> String
showRanks =
  V.foldl1 (++) . V.map (show . rank)


showGenomes :: Pool -> String
showGenomes =
  V.foldl1 (++) . V.map (show . genome)


-- Create a new pool with @nInds@ individuals with random genomes of length
-- @genomeLen@
newPool :: (MonadRandom m) => Int -> Int -> m Pool
newPool nInds genomeLen = V.replicateM nInds (newInd genomeLen)


-- Create a new individual with a random genome of length @genomeLen@ and other
-- values set to defaults
newInd :: (MonadRandom m) => Int -> m Ind
newInd genomeLen = do
  genome' <- randVector genomeLen
  return (Ind defaultFits genome' defaultRank defaultCdist)


-- Set rank of an individual and return the pool with the updated individual
setRank :: (MonadRandom m) => Int -> Int -> m Pool -> m Pool
setRank idx rank' pool = do
  p <- pool
  let updated_ind = (p V.! idx) { rank = rank' }
      np = V.update p (V.fromList [(idx, updated_ind)])
  return np

setRank' :: Int -> Int -> Pool -> Pool
setRank' idx rank' pool = V.update pool (V.fromList [(idx, updated_ind)])
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
