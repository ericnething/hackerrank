module RotateMatrix where

import Control.Monad
import Control.Monad.ST

import Data.Vector.Mutable
import qualified Data.Vector as V

----------------------------------------------------------------------
-- * Rotate a square matrix 90 degrees clockwise using ST to
-- completely encapsulate the mutations
----------------------------------------------------------------------

transposeST :: Int -> V.Vector a -> V.Vector a
transposeST dim v = runST $ do
  mv <- V.thaw v
  let end = dim - 1
      bounds = [(x,y) | y <- [0..end], x <- [y..end]]
  forM_ bounds $ \(x,y) ->
    swap mv (y*dim + x) (x*dim + y)
  V.freeze mv

reverseRowsST :: Int -> V.Vector a -> V.Vector a
reverseRowsST dim v = runST $ do
  mv <- V.thaw v
  let end = dim-1
  forM_ [0..end] $ \y ->
    forM_ [0..end `div` 2] $ \x ->
      swap mv (y*dim + x) (y*dim + (end - x))
  V.freeze mv

main :: IO ()
main = do
  let dim = 3
      example = V.generate (dim*dim) (+1)
  print example
  print . reverseRowsST dim . transposeST dim $ example

----------------------------------------------------------------------
-- * Transpose an immutable Vector without ST
----------------------------------------------------------------------

transposePure :: Int -> V.Vector a -> V.Vector a
transposePure dim v = V.generate (dim*dim) $ \i ->
  let {(x,y) = quotRem i dim}
  in v V.! (y*dim + x)

----------------------------------------------------------------------
-- * Another version using ST explicitly
----------------------------------------------------------------------

transpose2 :: Int -> V.Vector a -> ST s (V.Vector a)
transpose2 dim v = do
  mv <- V.thaw v
  let end = dim - 1
      bounds = [(x,y) | y <- [0..end], x <- [y..end]]
  forM_ bounds $ \(x,y) ->
    swap mv (y*dim + x) (x*dim + y)
  V.freeze mv

reverseRows2 :: Int -> V.Vector a -> ST s (V.Vector a)
reverseRows2 dim v = do
  mv <- V.thaw v
  let end = dim-1
  forM_ [0..end] $ \y ->
    forM_ [0..end `div` 2] $ \x ->
      swap mv (y*dim + x) (y*dim + (end - x))
  V.freeze mv

main2 :: IO ()
main2 = do
  let dim = 3
      example = V.generate (dim*dim) (+1)
  print example
  print $ runST $ reverseRows2 dim =<< transpose2 dim example
