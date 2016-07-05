module ZeroMatrix where

import Control.Monad
import Control.Monad.ST

import Data.List

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Mutable

setColumnAndRow :: (Int, Int) -> (Int, Int) -> Vector Int -> Int -> Vector Int
setColumnAndRow (dimy, dimx) (row, col) vec val = runST $ do
  mv <- V.thaw vec
  forM_ [0..dimy-1] $ \y ->
    write mv (y*dimx + col) val
  forM_ [0..dimx-1] $ \x ->
    write mv (row*dimx + x) val
  V.freeze mv

getZeroes :: (Int, Int) -> Vector Int -> [(Int, Int)]
getZeroes (dimy, dimx) vec = foldr g [] (zip [0..] (V.toList vec))
  where g (i, x) acc | x == 0 = (quot i dimx, rem i dimx) : acc
                     | otherwise = acc

zeroOutMatrix :: (Int, Int) -> Vector Int -> Vector Int
zeroOutMatrix dim v = foldr g v $ getZeroes dim v
  where g ix acc = setColumnAndRow dim ix acc 0

main :: IO ()
main = do
  let v = V.fromList [ 1, 0, 3, 4
                     , 5, 6, 0, 8
                     , 9,10,11,12
                     ,13,14,15,16 ]
      dim = (4, 4)
  print v
  print $ zeroOutMatrix dim v
  
