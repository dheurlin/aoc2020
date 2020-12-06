import Data.List.Split ( splitOn )
import Data.List       ( nub )

paragraphs :: String -> [[String]]
paragraphs = splitOn [""] . lines

countGroupAny :: [String] -> Int
countGroupAny = length . nub . concat

countGroupAll :: [String] -> Int
countGroupAll = length . commonElements

commonElements :: Eq a =>  [[a]] -> [a]
commonElements = foldl1 commonElts
  where commonElts l1 = filter (`elem` l1)

main :: IO ()
main = do putStrLn . ("Star 1: " <>) . show =<< ans1
          putStrLn . ("Star 2: " <>) . show =<< ans2
  where
    input = paragraphs <$> readFile "input"
    ans1 = sum . map countGroupAny <$> input
    ans2 = sum . map countGroupAll <$> input
