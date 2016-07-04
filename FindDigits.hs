module FindDigits where

import Control.Applicative

divisibleByDigits :: Int -> Int
divisibleByDigits n = go n
  where go 0 = 0
        go k | d == 0 = go rest
             | otherwise = result + go rest
          where (rest, d) = k `divMod` 10
                result | n `mod` d == 0 = 1
                       | otherwise      = 0

run :: IO ()
run = do
  n <- (read :: String -> Int) <$> getLine
  print $ divisibleByDigits n

main :: IO ()
main = do
  n <- readLn :: IO Int
  mapM_ (\_ -> run) [1..n]
