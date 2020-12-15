{-#  LANGUAGE ParallelListComp #-}

import Debug.Trace
import Data.Maybe
import Data.Array

-- type Grid = [[Char]]
type Grid = Array Int (Array Int Char)

neighbors :: Int -> Int -> Grid ->  [Char]
neighbors x y g = nRow (y - 1) <> nRow (y + 1) <> nX (x - 1) y <> nX (x + 1) y
  where
    nX x y | x < 0                   = []
           | x > length (g ! 0) - 1  = []
           | otherwise = [g ! y ! x]
    nRow y | y < 0             = []
           | y > length g - 1  = []
           | otherwise         = nX (x - 1) y <> nX x y <> nX (x + 1) y

findSeat :: Int -> Int -> Grid -> Int -> Int -> Maybe Char
findSeat x y g xOffs yOffs = findSeat' (x + xOffs) (y + yOffs)
  where
    findSeat' x y
      | x > length (g ! 0) - 1 || x < 0 || y > length g - 1 || y < 0 = Nothing
      | g ! y ! x /= '.' = Just $ g ! y ! x
      | otherwise = findSeat' (x + xOffs) (y + yOffs)

neighborSeats :: Int -> Int -> Grid -> [Char]
neighborSeats x y g = catMaybes [ findSeat x y g x' y'
                                | x' <- [-1..1], y' <- [-1..1]
                                , not (x' == y' && x' == 0) ]

occupiedAdj :: Int -> Int -> Grid -> Int
occupiedAdj x y g = length . filter (== '#') $ neighbors x y g

occupiedAdjSeats :: Int -> Int -> Grid -> Int
occupiedAdjSeats x y g = length . filter (== '#') $ neighborSeats x y g

change :: Int -> Int -> Grid -> Char
change x y g = case g ! y ! x of
  '.' -> '.'
  'L' | occupiedAdj x y g == 0 -> '#'
      | otherwise              -> 'L'
  '#' | occupiedAdj x y g >= 4 -> 'L'
  '#' | otherwise              -> '#'

change2 :: Int -> Int -> Grid -> Char
change2 x y g = case g ! y ! x of
  '.' -> '.'
  'L' | occupiedAdjSeats x y g == 0 -> '#'
      | otherwise                   -> 'L'
  '#' | occupiedAdjSeats x y g >= 5 -> 'L'
  '#' | otherwise                   -> '#'

step :: (Int -> Int -> Grid -> Char) -> Grid -> Grid
step change g = listArray (bounds g) [ listArray (bounds $ g ! 0) [ change x y g | x <- [0..length (g ! 0) - 1]] | y <- [0..length g - 1] ]

stepTillStable :: (Int -> Int -> Grid -> Char) -> Grid -> Grid
stepTillStable change g
  | newGrid == g = g
  | otherwise    = stepTillStable change newGrid
  where newGrid = step change g

star1 :: Grid -> Int
star1 = length . filter (== '#') . concatMap elems . elems
        . stepTillStable change

star2 :: Grid -> Int
star2 = length . filter (== '#') . concatMap elems . elems
        . stepTillStable change2

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . star1 =<< input
  putStrLn . ("Star 2: " <>) . show . star2 =<< input
    -- where input = lines <$> readFile "input"
    where input = do
          raw <- lines <$> readFile "input"
          pure $ listArray (0, length raw - 1)
            $ map (listArray (0, length (head raw) - 1)) raw



g = lines <$> readFile "testGrid"
n = lines <$> readFile "testNone"
