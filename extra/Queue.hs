module Queue where

import Prelude hiding (head, tail)

data Queue a = Queue [a] [a]
             deriving Show

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False

snoc :: Queue a -> a -> Queue a
snoc (Queue [] []) x = Queue [x] []
snoc (Queue fs rs) x = Queue fs (x:rs)

enqueue = snoc
(|>) = snoc

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue [] [])     = (Nothing, Queue [] [])
dequeue (Queue (f:[]) rs) = (Just f,  Queue (reverse rs) [])
dequeue (Queue (f:fs) rs) = (Just f,  Queue fs rs)

head :: Queue a -> Maybe a
head (Queue [] []) = Nothing
head (Queue (f:[]) rs) = Just f
head (Queue (f:fs) rs) = Just f

tail :: Queue a -> Maybe (Queue a)
tail (Queue [] []) = Nothing
tail (Queue (f:[]) rs) = Just $ Queue (reverse rs) []
tail (Queue (f:fs) rs) = Just $ Queue fs rs

fromList :: [a] -> Queue a
fromList fs = Queue fs []

toList :: Queue a -> [a]
toList (Queue fs rs) = fs ++ reverse rs
