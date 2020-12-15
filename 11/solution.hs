{-#  LANGUAGE ParallelListComp #-}

import Debug.Trace
import Data.Maybe

type Grid = [[Char]]

sHead :: [a] -> Maybe a
sHead [] = Nothing
sHead xs = Just $ head xs

sLast :: [a] -> Maybe a
sLast [] = Nothing
sLast xs = Just $ last xs

neighbors :: Int -> Int -> Grid ->  [Char]
neighbors x y g = nRow (y - 1) <> nRow (y + 1) <> nX (x - 1) y <> nX (x + 1) y
  where
    nX x y | x < 0                    = []
           | x > length (head g) - 1  = []
           | otherwise = [g !! y !! x]
    nRow y | y < 0             = []
           | y > length g - 1  = []
           | otherwise         = nX (x - 1) y <> nX x y <> nX (x + 1) y

neighborSeats :: Int -> Int -> Grid -> [Char]
neighborSeats x y g = catMaybes [ fstE , fstW , fstN , fstS
                                , fstNE, fstNW, fstSE, fstSW ]
-- neighborSeats x y g = catMaybes [ fstNE ]
  where
    fstW  = sLast . notEmpty . take (x - 1) $ g !! y
    fstE  = sHead . notEmpty . drop (x + 1) $ g !! y
    fstN  = sLast . notEmpty . map (!! x) . take (y - 1) $  g
    fstS  = sHead . notEmpty . map (!! x) . drop (y + 1) $  g
    fstNE = sHead . notEmpty
            $ [ g !! c !! r | c <- [y-1..0], c > 0 | r <- [x+1..length (head g) - 1]]
    fstNW = sHead . notEmpty
            $ [ g !! c !! r | c <- [y-1..0], c > 0 | r <- [x-1..0], r > 0 ]
    fstSE = sHead . notEmpty
            $ [ g !! c !! r | c <- [y+1..length g - 1] | r <- [x+1..length (head g) - 1]]
    fstSW = sHead . notEmpty
            $ [ g !! c !! r | c <- [y+1..length g - 1] | r <- [x-1..0], r > 0]
    notEmpty = filter (/= '.')

occupiedAdj :: Int -> Int -> Grid -> Int
occupiedAdj x y g = length . filter (== '#') $ neighbors x y g

change :: Int -> Int -> Grid -> Char
change x y g = case g !! y !! x of
  '.' -> '.'
  'L' | occupiedAdj x y g == 0 -> '#'
      | otherwise              -> 'L'
  '#' | occupiedAdj x y g >= 4 -> 'L'
  '#' | otherwise              -> '#'

step :: Grid -> Grid
step g = [ [ change x y g | x <- [0..length (head g) - 1]]
           | y <- [0..length g - 1] ]

stepTillStable :: Grid -> Grid
stepTillStable g
  | newGrid == g = g
  | otherwise    = stepTillStable newGrid
  where newGrid = step g

star1 :: Grid -> Int
star1 = length . filter (== '#') . concat . stepTillStable

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . star1 =<< input
    where input = lines <$> readFile "input"

g = lines <$> readFile "testGrid"
n = lines <$> readFile "testNone"
