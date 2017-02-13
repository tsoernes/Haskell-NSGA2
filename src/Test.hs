module Test (
) where

import           Control.Monad
import           Control.Monad.Random        as MR
import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU


data Obj = Obj
    { aNum :: Int
    , aVec  :: (VU.Vector Int)
    }

instance Show Obj where
   show Obj{ aNum = a
           , aVec = v
           } = show a ++ show v -- 'show v' not OK

main :: IO()
main = do
  rVec <- evalRandIO (randVector 5) -- OK
  obj <- evalRandIO (newObj 1 5) -- Not OK
  print $ show rVec
  print $ show obj

newObj :: (MonadRandom m) => Int -> Int -> m Obj
--newObj aNum' vecLen = Obj aNum' $ randVector vecLen
newObj n vecLen = do
  rvec <- randVector vecLen
  return (Obj n rvec)

randVector :: (MonadRandom m) => Int -> m (VU.Vector Int)
randVector len = randSample (VU.enumFromN 0 len) len

-- Fisher-yates shuffle
randSample :: (MonadRandom m, (VG.Vector v a)) => v a -> Int -> m (v a)
randSample vec len = do
  let getR i = do
          r <- getRandomR (i, (VG.length vec)-1)
          return (i, r)

  swaps <- mapM getR [0..len-1]

  let vec_rands = runST $ do
          vec_mut <- VG.thaw vec
          forM_ swaps $ \(i, j) -> do
              VGM.swap vec_mut i j
          vec_rands' <- VG.unsafeFreeze vec_mut
          return vec_rands'

  return $ VG.take len vec_rands
