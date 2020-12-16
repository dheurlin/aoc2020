import qualified Data.Map as M
import Control.Monad

type Numbers = M.Map Int (Maybe Int)

step :: (Int, Numbers) -> Int -> (Int, Numbers)
step (last, ns) num =
  let newNum = maybe 0 ((num - 1) - ) (join $ M.lookup last ns)
  in (newNum, M.insert last (Just (num - 1)) ns)

mkMap :: [Int] -> Numbers
mkMap xs = M.fromList [ (x, Just n) | (x, n) <- zip xs [1..]]

solve :: Int -> [Int] -> Int
solve to input = fst $ foldl step (last input, map) [length input + 1..to]
  where map = mkMap input

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . solve 2020 $ myInput
  putStrLn . ("Star 2: " <>) . show . solve 30000000 $ myInput
    where
      myInput = [11,18,0,20,1,7,16]
