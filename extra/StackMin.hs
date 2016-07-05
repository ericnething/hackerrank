module StackMin where

import Data.List

-- Stack [(value, min)]
data Stack a = Stack [(a, a)]
             deriving Show

push :: Ord a => a -> Stack a -> Stack a
push a (Stack []) = Stack [(a, a)]
push a (Stack ((x, i):xs)) = Stack ((a, min a i):(x, i):xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack ((x, _):xs)) = Just (x, Stack xs)
pop (Stack []) = Nothing

getMin :: Stack a -> Maybe a
getMin (Stack ((x, i):xs)) = Just i
getMin (Stack []) = Nothing

main :: IO ()
main = do
  let s = push 6 $ push 3 $ push 4 $ push 5 (Stack [])

  putStrLn $ "The stack is: " ++ show s
  putStrLn $ "Min: " ++ maybe ("empty") show (getMin s)
  putStrLn $ "Pop: " ++ maybe ("empty") (show . fst) (pop s)

  let s' = maybe (Stack []) snd (pop s)
  
  putStrLn $ "Min: " ++ maybe ("empty") show (getMin s')
  putStrLn $ "New Stack: " ++ show s'

  putStrLn $ "Pop: " ++ maybe ("empty") (show . fst) (pop s')
  
  let s'' = maybe (Stack []) snd (pop s')

  putStrLn $ "Min: " ++ maybe ("empty") show (getMin s'')
  putStrLn $ "New Stack: " ++ show s''
  
