module TestParentSelection (

) where


import qualified Data.Vector   as V
import           Prelude       as P
import           System.Random

import           Genome
import           RandUtils
import           SortUtils     (indCmpCrowded)


-- No replacement
tournamentSelect :: (RandomGen g) => Pool -> Int -> g-> (Pool, g)
tournamentSelect = undefined
