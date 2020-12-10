import Data.List

diffs :: [Int] -> [Int]
diffs [a,b] = [b - a]
diffs (a:b:xs) = diffs [a,b] ++ diffs (b:xs)

joltagesAll :: [Int] -> [Int]
joltagesAll xs = 0 : sorted <> [3 + last sorted]
  where sorted = sort xs

choose :: Int -> Int -> Int
n `choose` k = product [1+n-k..n] `div` product [1..k]

-- The number of ways to remove adapters from a sequence with the same distance
possibleInGroup :: [Int] -> Int
possibleInGroup xs@(1:_) = (length xs `choose` 2) + 1
possibleInGroup _ = 1

star1 :: [Int] -> Int
star1 = product . map length
          . filter (\xs -> head xs == 1 || head xs == 3)
          . group . sort . diffs . joltagesAll

star2 :: [Int] -> Int
star2 = product . map possibleInGroup . group . diffs . joltagesAll

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . star1 =<< input
  putStrLn . ("Star 2: " <>) . show . star2 =<< input
    where input = map read . lines <$> readFile "input"
