module Rubbish (
) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import           Genome


data Ind a b = Ind
    { fitnesses :: VU.Vector a
    , genome    :: VU.Vector b
    , rank      :: Int -- ^ Equals the index of the non-dominated (pareto) front, i.e. lower is better
    , cdist     :: Double -- ^ Crowding distance
    } deriving (Eq, Show)

type Fitness = Double

a = V.fromList [10::Fitness, 15]
b = V.fromList [12::Fitness, 15]
strictly_better_cmp = V.fromList [(<)::Fitness->Fitness->Bool, (<)]
no_worse_cmp = V.fromList [(<=)::Fitness->Fitness->Bool, (<=)]

strictly_better = V.zipWith3 ($) strictly_better_cmp a b
no_worse = V.zipWith3 ($) no_worse_cmp a b

main :: IO()
main = do
  print strictly_better
  print no_worse
