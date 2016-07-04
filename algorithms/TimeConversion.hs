import System.IO

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f xs = go [] xs
  where go ys (x:xs)
          | f x = (reverse ys, (x:xs))
          | otherwise = go (x:ys) xs
        go ys [] = (reverse ys, [])

break2 :: (a -> Bool) -> [a] -> ([a], [a])
break2 f xs = go xs
  where go (x:xs)
          | f x = ([], x:xs)
          | otherwise = let (ys, zs) = go xs in (x:ys, zs)
        go xs@([]) = (xs, xs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c xs = go [] xs
  where go result [] = reverse result
        go result xs = let (left, right) = break' (== c) xs
                       in go (left : result) right

convertTo24Time :: String -> Maybe String
convertTo24Time xs
  | modifier == "PM" = Just $ format (hour, rest)
  | modifier == "AM" = Just $ check hour ++ rest
  where modifier = drop (length xs - 2) xs
        (hour, rest) = break' (== ':') $ take (length xs - 2) xs
        check h | h == "12" = "00"
                | otherwise = h
        format (hour, rest) = (show . (+ 12) . (`mod` 12) $ toInt hour) ++ rest
        toInt = (read :: String -> Int)
convertTo24Time _ = Nothing

main :: IO ()
main = putStrLn . maybe "Error" id . convertTo24Time =<< getLine
