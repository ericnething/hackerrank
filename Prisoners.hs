import Control.Monad
import Control.Applicative
import System.IO

getLastId :: (Int, Int, Int) -> Int
getLastId (prisoners, sweets, index) = check n
  where n       = (sweets + index - 1) `mod` prisoners
        check 0 = prisoners
        check n = n

main :: IO ()
main = do
  n <- readLn
  let f :: String -> (Int, Int, Int)
      f = (\(p:s:i:_) -> (p,s,i)) . fmap (read :: String -> Int) . words
  mapM_ (putStrLn . show . getLastId . f) =<< replicateM n getLine
