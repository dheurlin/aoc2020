import Debug.Trace

parseInput :: String -> ((Int, Int), [Int])
parseInput s = ((minimum ns, maximum ns), ns)
  where ns = map (read . (: [])) s

-- Keep invariant: current cup always head!

pickUp :: [Int] -> ([Int], [Int])
pickUp (n:ns) = (picked, rest)
  where
    picked = take 3 ns
    rest = n : drop 3 ns

insert :: [Int] -> [Int] -> [Int]
insert picked (n:ns) = n : picked <> ns

select :: Int -> [Int] -> [Int]
select i ns = take (length ns) $ dropWhile (/= i) $ cycle ns

selectAfter :: Int -> [Int] -> [Int]
selectAfter i ns = take (length ns) $ tail $ dropWhile (/= i) $ cycle ns

findNext :: Int -> [Int] -> (Int, Int) -> Int
findNext curr banned (mn,mx) = go (curr-1)
  where
    go curr
      | curr < mn          = go mx
      | curr `elem` banned = go (curr - 1)
      | otherwise          = curr


play :: (Int, Int) -> [Int] -> [Int]
play bounds ns =
  let (picked, rest) = pickUp ns
      dest           = findNext (head ns) picked bounds
      newRest        = select dest rest
      inserted       = insert picked newRest
  in selectAfter (head ns) inserted

star1 :: (Int, Int) -> [Int] -> String
star1 bounds ns = concatMap show . tail . select 1
                $ iterate (play bounds) ns !! 100

star2 :: (Int, Int) -> [Int] -> Int
star2 bounds ns = (res !! 1) * (res !! 2)
  where
    res = select 1 $ iterate (play bounds) ns !! 10000000

-- TODO it seems the big input is what's making it slow, not (just) the number
-- of iterations. It would be nice to profile to see what's taking time
-- _
-- slow because it's going backward when selecting indices?
main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . uncurry star1 =<< input
  putStrLn . ("Star 2: " <>) . show . uncurry star2 =<< input2
  -- putStrLn . uncurry star1 =<< input
    where
      input = parseInput . filter (/= '\n') <$> readFile "testInput"
      input2 = do
        ((mn,mx),ns) <- input
        pure ((mn,1000000) ,ns <> [mx+1..1000000])
