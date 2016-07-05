module StringRotation where

import Data.List

isRotationOf :: String -> String -> Bool
isRotationOf s1 s2 | sameLength = s1 `isInfixOf` (s2 ++ s2)
                   | otherwise = False
  where sameLength = (length s1 == length s2) && (length s1 > 0)

main :: IO ()
main = do
  let s1 = "terbottlewa"
      s2 = "waterbottle"
  putStrLn $ show $ s1 `isRotationOf` s2
  
