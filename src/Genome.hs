module Genome (
  Ind(fitnesses, genome, rank, cdist), Pool, Fronts,
  newInd, setRankP, setCdistP, setCdistI, getFit
) where

import           Control.Monad
import           Control.Monad.ST
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
   show Ind{ fitnesses = f, rank = r } = show f ++ "r" ++ show r


-- Create a new individual with a random genome.
newInd :: (RandomGen g) => Int -> g -> (Ind, g)
newInd n rgen =
  (Ind (VU.fromList [-1, -1]) genome (-1) (-1), rgen')
    where
  (genome, rgen') = randVector n rgen


setRankP :: Int -> Int -> Pool -> Pool
setRankP idx rank' pool =
  V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { rank = rank' }


setCdistP :: Int -> Float -> Pool -> Pool
setCdistP idx cdist' pool =
  V.update pool (V.fromList [(idx, updated_ind)])
    where
  updated_ind = (pool V.! idx) { cdist = cdist' }


setCdistI :: Ind -> Float -> Ind
setCdistI ind cdist' =
  ind { cdist = cdist' }


getFit :: Pool -> Int -> Int -> Float
getFit pool indIdx fitIdx =
  fitnesses (pool V.! indIdx) VU.! fitIdx
