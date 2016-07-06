module BalancedBrackets where

import System.IO (getLine)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.List (foldl')
import Prelude hiding (null)

data Stack a = Stack [a] deriving Show

empty :: Stack a
empty = Stack []

null :: Stack a -> Bool
null (Stack []) = True
null _          = False

push :: Ord a => a -> Stack a -> Stack a
push a (Stack []) = Stack [a]
push a (Stack xs) = Stack (a:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

isBalanced :: String -> Bool
isBalanced = go empty
  where go stack [] | null stack = True
                    | otherwise  = False
        go stack (x:xs) = case update x stack of
          Right stack' -> go stack' xs
          Left _       -> False

        update x stack = case x of
          '(' -> Right $ push ')' stack
          '{' -> Right $ push '}' stack
          '[' -> Right $ push ']' stack
          ')' -> check ')' stack
          '}' -> check '}' stack
          ']' -> check ']' stack
          _   -> Left "Input is invalid"

        check _ (Stack []) = Left "Empty stack"
        check val stack@(Stack (x:xs))
          | val == x  = Right $ snd (pop stack)
          | otherwise = Left "Mismatched brackets"

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  n <- readInt <$> getLine
  result <- fmap isBalanced <$> replicateM n getLine
  mapM_ (putStrLn . (\a -> case a of True -> "YES"; _ -> "NO")) result
