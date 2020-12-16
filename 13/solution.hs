import Data.List
import Data.List.Split
import Text.Read
import Data.Maybe
import Math.NumberTheory.Moduli.Chinese

-- Star 1 ----------------------------------------------------------------------

type TimeTable = (Int, [Int])

mkTable :: Int -> (Int, [Int])
mkTable i = (i, iterate (+i) 0)

earliest :: Int -> TimeTable -> (Int, Int)
earliest after (busId, times) = (busId, head $ dropWhile (< after) times)

earliestAll :: Int -> [TimeTable] -> (Int, Int)
earliestAll after tables = head $ sortOn snd $ map (earliest after) tables

star1 :: Int -> [TimeTable] -> Int
star1 after tables = busId * (time - after)
  where (busId, time) = earliestAll after tables

-- Star 2 ----------------------------------------------------------------------

parseTables :: [String] -> [TimeTable]
parseTables = map (mkTable . read) . filter (/= "x") . splitOn "," . last

parseIds :: String -> [(Integer, Integer)]
parseIds s = [ (n, fromJust t) | (n, t) <- zip [l,l-1..0] mbyIds, isJust t ]
  where
    mbyIds = map readMaybe $ splitOn "," s
    l = fromIntegral $ length mbyIds - 1


star2 :: [(Integer, Integer)] -> Maybe Integer
star2 dudes = subtract (fst . head $ dudes) <$> chineseRemainder dudes

-- Solution --------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . uncurry star1 =<< input
  putStrLn . ("Star 2: " <>) . show . star2
                             . parseIds . last . lines =<< readFile "input"
    where
      input :: IO (Int, [TimeTable])
      input = do
        file <- lines <$> readFile "input"
        let after = read (head file)
            tables = parseTables file
        pure (after, tables)
