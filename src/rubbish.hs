import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import           Genome


main :: IO()
main = do
  aNum <- if True
          then getRandomR(0::Int,10)
          else return 3
  print aNum
