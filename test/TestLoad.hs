module TestLoad (
) where

import qualified Data.Vector as V
import           Load

test :: IO()
test = do
  ds <- load1
  V.mapM_ print $ cost ds
