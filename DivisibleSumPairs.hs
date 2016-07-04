module DivisibleSumPairs where

import           Control.Applicative
import           System.IO
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))

generatePairs :: Int -> Int -> Vector Int -> [(Int, Int)]
generatePairs n k v = [(y, x) | y <- [0..n-1]
                              , x <- [y..n-1]
                              , y < x
                              , ((v ! y) + (v ! x)) `mod` k == 0]

main :: IO ()
main = do
  (n:k:_) <- fmap (read :: String -> Int) . words <$> getLine
  array <- V.fromList . fmap (read :: String -> Int) . words <$> getLine
  putStrLn $ show $ length (generatePairs n k array)
