module InsertionSort1 where

import           Control.Monad.ST
import           Control.Monad
import           Control.Applicative
import           System.IO
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import qualified Data.Vector.Mutable as MV
import           Data.Vector.Mutable (IOVector)

-- | For a sorted array (except for the last element), move the last
-- element into place, recording the state of the array at each step.
-- insert :: (Ord a, Eq a) => Int -> IOVector a -> IOVector a
insert size vec e index = do
  e <- read mv (size - 1)
  loop 
  
    where loop

readArray :: String -> Vector Int
readArray = V.fromList . fmap readInt . words

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  n     <- readInt   <$> getLine
  array <- V.thaw . readArray <$> getLine
  result <- insertion n array

