import Debug.Trace

-- (Before, Current, After)
type GameState = ([Int], Int, [Int])

parseInput':: String -> ((Int, Int), GameState)
parseInput' s = ((minimum ns, maximum ns), ([], n, ns))
  where (n:ns) = map (read . (: [])) s

pickUp :: GameState -> ([Int], GameState)
pickUp (bef, curr, aft) = (p1 <> p2, (r2, curr, r1))
  where
    (p1, r1) = splitAt 3 aft
    (p2, r2) = splitAt (3 - length p1) bef

insert :: [Int] -> GameState -> GameState
insert picked (bef, curr, aft) = (bef, curr, picked <> aft)

rot1 :: GameState -> GameState
rot1 (bef, curr, [])  = ([curr], head bef, tail bef)
rot1 (bef, curr, aft) = (bef <> [curr], head aft, tail aft)

select :: Int -> GameState -> GameState
select i (bef, curr, aft)
  | curr == i = (bef, curr, aft)
  | otherwise = select i $ rot1 (bef, curr, aft)

selectAfter :: Int -> GameState -> GameState
selectAfter i s = rot1 $ select i s

findNext :: Int -> [Int] -> (Int, Int) -> Int
findNext curr banned (mn,mx) = go (curr-1)
  where
    go curr
      | curr < mn          = go mx
      | curr `elem` banned = go (curr - 1)
      | otherwise          = curr

play :: (Int, Int) -> GameState -> GameState
play bounds ns =
  let (picked, rest@(_,curr,_)) = pickUp ns
      dest     = findNext curr picked bounds
      newRest  = select dest rest
      inserted = insert picked newRest
  in selectAfter curr inserted

star1 :: (Int, Int) -> GameState -> String
star1 bounds s = concatMap show $ aft <> bef
  where (bef, _, aft) = select 1 $ iterate (play bounds) s !! 100

star2 :: (Int, Int) -> GameState -> Int
star2 bounds s = product $ take 2 aft
  where
    (_, _, aft) = select 1 $ iterate (play bounds) s !! 10000000

-- Star 1: 29385746
main :: IO ()
main = do
  -- putStrLn . ("Star 1: " <>) . uncurry star1 =<< input
  putStrLn . ("Star 2: " <>) . show . uncurry star2 =<< input2
    where
      input = parseInput' . filter (/= '\n') <$> readFile "input"
      input2 = do
        ((mn,mx),(bef, curr, aft)) <- input
        pure ((mn,1000000) ,(bef, curr, aft <> [mx+1..1000000]))
