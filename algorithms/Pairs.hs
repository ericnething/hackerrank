module Pairs where

import           Control.Applicative
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import qualified Data.List as L


-- | Given a Vector `a`, for each index `i`, compare `a_i` to
-- every `a_j` where `i /= j`. If `a_i - a_j == k` for some value `k`,
-- add it to the list of valid pairs.
--
-- Optimizations:
--
-- Vector `a` is sorted.
-- 
-- (Upper bounds): If 'j >= i', stop and move onto the next `i`.
--
-- (Lower bounds): If `a_i - a_j > k`, stop and move onto the next `i`.
pairs :: Int -> Vector Int -> [(Int, Int)]
pairs k xs = V.ifoldr g [] xs
  where
    -- Accumulate all of the valid pairs into a list. Start each
    -- iteration at `i-1` to exclude `a_i`.
    g i _ acc = step (i-1) ++ acc
      where
        -- Iterate over the indexes in reverse from `j` to the lower
        -- bound.
        step j
          -- `j` is out of the array bounds.
          | j < 0 = []
    
          -- Lower bound reached (optimization)
          | (xs ! i) - (xs ! j) > k = []
                                      
          -- Found a valid pair. Add it to the list and continue.
          | (xs ! i) - (xs ! j) == k = (xs ! i, xs ! j) : step (j - 1)
        
          -- Otherwise, continue.
          | otherwise = step (j - 1)
    
main :: IO ()
main = do
  (n:k:_) <- fmap (read :: String -> Int) . words <$> getLine
  array <- V.fromList . L.sort . fmap (read :: String -> Int) . words <$> getLine
  print $ length (pairs k array)
