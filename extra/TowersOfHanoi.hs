module TowersOfHanoi where

import Data.List

-- You have three pegs (A B C). N disks are on peg A. Move all of the
-- disks from A to C.

hanoi :: Int -> a -> a -> a -> [(a,a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

main :: IO ()
main = mapM_ g $ hanoi 5 "A" "B" "C"
  where g (x,y) = putStrLn $ "Move " ++ show x ++ " to " ++ show y
