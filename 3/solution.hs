
type Grid = [[Char]]

numTrees :: (Int, Int) -> Grid -> Integer
numTrees (right, down) rows = fst  . ( !! (length rows `div` down))
                                $ iterate step (0, rows)
  where
    step :: (Integer, Grid) -> (Integer, Grid)
    step (currentNum, grid)
      | (head . head) grid == '#' = (currentNum + 1, shiftMap grid)
      | otherwise                 = (currentNum    , shiftMap grid)

    shiftMap  = drop down . map (drop right)

main :: IO ()
main = do putStrLn . ("Silver: " <>) . show . numTrees (3,1)   =<< grid
          putStrLn . ("Gold:   " <>) . show . allSlopes        =<< grid
  where
    grid = map cycle . lines <$> readFile "input"
    slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    allSlopes rows = product [numTrees slope rows | slope <- slopes]
