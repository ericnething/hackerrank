module ArrayHourglass where

import           Control.Applicative
import           Control.Monad
import           System.IO
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))

getHourglass :: Int -> Vector Int -> (Int, Int) -> [Int]
getHourglass size vec (y,x) = foldr g [] indices
  where indices = [(y,x),(y,x+1),(y,x+2),(y+1,x+1),(y+2,x),(y+2,x+1),(y+2,x+2)]
        g (y,x) acc = vec ! (y * size + x) : acc

findMaxHourglassSum :: Int -> Vector Int -> Int
findMaxHourglassSum size vec = maximum $ fmap (sum . getHourglass size vec) indices
  where indices = [ (y, x) | y <- [0..size-3], x <- [0..size-3]]

readArray :: String -> Vector Int
readArray = V.fromList . fmap (read :: String -> Int) . words

main :: IO ()
main = do
  array <- readArray . unwords <$> replicateM 6 getLine
  print $ findMaxHourglassSum 6 array
