import Control.Monad
import Data.Array.IO

-- Need to use mutable arrays or part 2 explodes. Is still slow af though

type NumbersArr = IOArray Int (Maybe Int)

step :: (Int, NumbersArr) -> Int -> IO (Int, NumbersArr)
step (last, ns) num = do
  newNum <- maybe 0 ((num - 1) - ) <$> readArray ns last
  writeArray ns last $ Just (num - 1)
  pure (newNum, ns)

solve :: Int -> [Int] -> IO Int
solve to input = do
  nums <- newArray (0, to) Nothing
  forM_ (zip input [1..]) $ \(ix, e) -> writeArray nums ix (Just e)
  fst <$> foldM step (last input, nums) [length input + 1..to]

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show =<< solve 2020 myInput
  putStrLn . ("Star 2: " <>) . show =<< solve 30000000 myInput
    where
      myInput = [11,18,0,20,1,7,16]
