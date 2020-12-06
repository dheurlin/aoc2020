#! /bin/env runhaskell

import Data.List ( sort )

getPos :: (Int, Int) -> (Char, Char) -> String -> Int
getPos range codes s = fst $ getPos' s range codes
  where
    getPos' s range@(lower, upper) codes@(downCode, upCode)
      | lower == upper = range
      | head s == downCode
        = getPos' (tail s) (lower, lower + (upper - lower) `div` 2) codes
      | head s == upCode
        = getPos' (tail s) (upper - (upper - lower) `div` 2, upper) codes

getRow :: String -> Int
getRow = getPos (0, 127) ('F', 'B')

getCol :: String -> Int
getCol = getPos (0, 7) ('L', 'R')

seatID :: String -> Int
seatID s = 8 * getRow (take 7 s) + getCol (drop 7 s)

findFreeSeat :: [Int] -> Int
findFreeSeat = findFreeSeat' . sort
  where
    findFreeSeat' [_,_] = error "No free seat found!"
    findFreeSeat' (a:b:xs)
      | a + 2 == b = a + 1
      | otherwise  = findFreeSeat (b:xs)

main :: IO ()
main = do putStrLn . ("Star 1: " <>) . show =<< answer1
          putStrLn . ("Star 2: " <>) . show =<< answer2
  where
    seatIDs = map seatID . lines <$> readFile "input"
    answer1 = maximum      <$> seatIDs
    answer2 = findFreeSeat <$> seatIDs
