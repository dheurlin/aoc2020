
-- find a * b such that a + b = 2020

solve :: [Int] -> Int
solve nums = a * b * c
  where (a,b,c) = head [(a,b,c) | a <- nums, b <- nums, c <- nums, a + b + c == 2020]

main :: IO ()
main = readFile "input" >>= print . solve . map read . lines
