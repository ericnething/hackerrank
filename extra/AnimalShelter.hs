{-# LANGUAGE LambdaCase #-}

module AnimalShelter where

----------------------------------------------------------------------
-- * Standard FIFO Queue
----------------------------------------------------------------------

import Prelude hiding (head, tail)

data Queue a = Queue [a] [a]
             deriving Show

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null q = case q of
  Queue [] [] -> True
  _           -> False

snoc :: Queue a -> a -> Queue a
snoc (Queue [] []) x = Queue [x] []
snoc (Queue fs rs) x = Queue fs (x:rs)

enqueue = snoc
(|>) = snoc

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q = case q of
  Queue [] []     -> (Nothing, Queue [] [])
  Queue (f:[]) rs -> (Just f,  Queue (reverse rs) [])
  Queue (f:fs) rs -> (Just f,  Queue fs rs)

head :: Queue a -> Maybe a
head q = case q of
  Queue [] []     -> Nothing
  Queue (f:[]) rs ->  Just f
  Queue (f:fs) rs -> Just f

tail :: Queue a -> Maybe (Queue a)
tail q = case q of
  Queue [] [] -> Nothing
  Queue (f:[]) rs -> Just $ Queue (reverse rs) []
  Queue (f:fs) rs -> Just $ Queue fs rs

fromList :: [a] -> Queue a
fromList fs = Queue fs []

toList :: Queue a -> [a]
toList (Queue fs rs) = fs ++ reverse rs

----------------------------------------------------------------------
-- * Animal Shelter
----------------------------------------------------------------------

data Animal = Dog | Cat
            deriving (Show, Eq)

dequeueAnimal :: Animal -> Queue Animal -> (Maybe Animal, Queue Animal)
dequeueAnimal animal q = go [] (toList q)
  where
    -- not found
    go xs [] = (Nothing, fromList $ reverse xs)

    go xs (y:ys)
      -- found
      | y == animal = (Just y, fromList (reverse xs ++ ys))
      -- keep looking
      | otherwise   = go (y:xs) ys

dequeueDog = dequeueAnimal Dog
dequeueCat = dequeueAnimal Cat

main :: IO ()
main = do
  let q1 = fromList [Dog, Dog, Dog, Cat, Dog, Cat, Cat, Dog]
      q2 = fromList [Cat, Cat]

  putStrLn $ "\nFirst Queue: " ++ show (toList q1)
  putStrLn $ "Give me any animal: " ++ show (dequeue q1)
  putStrLn $ "Give me a dog: " ++ show (dequeueDog q1)
  putStrLn $ "Give me a cat: " ++ show (dequeueCat q1)

  putStrLn $ "\nSecond Queue: " ++ show (toList q2)
  putStrLn $ "Give me a dog: " ++ show (dequeueDog q2)
  
