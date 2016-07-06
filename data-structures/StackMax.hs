module StackMax where

import System.IO (getLine)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.List (foldl')

-- | Stack annotated with the maximum value at that point
data Stack a = Stack [(a,a)]
             deriving Show

empty :: Stack a
empty = Stack []

push :: Ord a => a -> Stack a -> Stack a
push a (Stack []) = Stack [(a, a)]
push a (Stack xs@(x:_))
  | a > snd x = Stack ((a,a):xs)
  | otherwise = Stack ((a, snd x):xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just $ fst x, Stack xs)

stackMax :: Stack a -> Maybe a
stackMax (Stack [])     = Nothing
stackMax (Stack (x:xs)) = Just $ snd x

processCommand :: (Stack Int, [Maybe Int]) -> String -> (Stack Int, [Maybe Int])
processCommand (stack, result) cmd = case cmd of
  '1':xs -> (push (readInt $ drop 1 xs) stack, result)
  "2"    -> (snd $ pop stack                 , result)
  "3"    -> (stack                           , stackMax stack : result)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  n <- readInt <$> getLine
  result <- catMaybes . reverse . snd . foldl' processCommand (empty,[])
            <$> replicateM n getLine
  mapM_ print result
