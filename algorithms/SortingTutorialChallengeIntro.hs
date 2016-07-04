module SortingTutorialChallengeIntro where

import           Control.Applicative
import           System.IO
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))

-- | Binary search
getIndex :: Int -> Int -> Vector Int -> Maybe Int
getIndex k size v = go (0, size - 1)
  where go (low, high)
        
          -- Not found
          | low > high     = Nothing
                             
          -- Found it
          | v ! index == k = Just index

          -- Go left
          | v ! index > k  = go (low, index - 1)

          -- Go right
          | v ! index < k  = go (index + 1, high)
                             
            where index = ((high - low) `div` 2 + low)

readArray :: String -> Vector Int
readArray = V.fromList . fmap readInt . words

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  k     <- readInt   <$> getLine
  n     <- readInt   <$> getLine
  array <- readArray <$> getLine
  putStrLn $ maybe ("-1") show (getIndex k n array)
