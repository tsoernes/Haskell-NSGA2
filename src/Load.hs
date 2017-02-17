module Load (
  loadDS, DataSet (cost, dist)
) where

import           Data.List.Split
import qualified Data.Vector     as V


data DSPath = DSPath
  { costP :: String
  , distP :: String
  }


data DataSet = DataSet
  { cost :: V.Vector (V.Vector Double)
  , dist :: V.Vector (V.Vector Double)
  }


dsPath1 :: DSPath
dsPath1 = DSPath "cost.txt" "distance.txt"
dsPath2 :: DSPath
dsPath2 = DSPath "euclidA100.tsp" "euclidB100.tsp"

test :: IO ()
test = do
  ds <- loadDS
  V.mapM_ print $ cost ds


loadDS :: IO DataSet
loadDS = do
  cost' <- readFile $ costP dsPath1
  dist' <- readFile $ distP dsPath1
  return $ DataSet (format cost') (format dist')


format :: String -> V.Vector (V.Vector Double)
format li =
  mirror asvec
    where
  rows = splitOn "\n" li
  cols = map (splitOn "\t") rows
  asint = map (map (\x -> if x == " " then (-1) else read x)) cols
  asvec = V.fromList $ map V.fromList asint


-- Mirror bottom left diagonal half to top right
mirror :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
mirror li =
  V.imap (\idx subli -> V.update subli $ idxs V.! idx) li
    where
  n = V.length li - 1
  get x y = (li V.! y) V.! x
  idxs = V.fromList [V.fromList [(j, get i j) | j <- [i+1..n]] | i <- [0..n]]
