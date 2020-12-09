import Data.Foldable
import Data.Maybe
import Data.List
import Control.Monad

-- Checks if a list contains two numbers that add to a given number
hasSum :: Int -> [Int] -> Bool
hasSum s xs = any (\x -> s - x `elem` xs) (tail xs)

-- Returns (head xs) if (tail xs) does *not* contain numbers that sum to (head xs)
headNotSum :: [Int] -> Maybe Int
headNotSum (x:xs) = x <$ guard (not $ hasSum x xs)

-- Returns all sublists of size `size` of xs
windows :: Int -> [Int] -> [[Int]]
windows size xs = [ drop n . take (size + n) $ xs | n <- [0..length xs - size] ]

firstNotSum :: Int -> [Int] -> Maybe Int
firstNotSum preamble xs
  = asum [ headNotSum (reverse l) | l <- windows (preamble + 1) xs ]

-- The first sequence of xs that adds to s
seqWithSum :: Int -> [Int] -> Maybe [Int]
seqWithSum s xs
  = asum [ initsHasSum s (inits $ drop n xs) | n <- [0 .. length xs] ]
  where
    initsHasSum s (xs:xss) = case sum xs of
      xSum | xSum > s  -> Nothing
           | xSum == s -> Just xs
           | otherwise -> initsHasSum s xss

addSeqWithSum :: Int -> [Int] -> Maybe Int
addSeqWithSum s xs = (\ns -> minimum ns + maximum ns) <$> seqWithSum s xs

main = do
  putStrLn . ("Star 1: " <>) . show =<< (fromJust <$> firstInvalid)
  putStrLn . ("Star 2: " <>) . show =<< (fromJust <$> rangeSum)
    where
      input        = map read . lines <$> readFile "input"
      firstInvalid = firstNotSum 25 <$> input
      rangeSum     = (\sum xs -> (`addSeqWithSum` xs) =<< sum)
                      <$> firstInvalid <*> input

