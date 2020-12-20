import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as M
import Data.List.Split
import Data.Either
import Control.Monad

data Rule  = Seq [Int]
           | Or Rule Rule
           | Eq Char
  deriving (Eq, Show)

parseRule :: String -> Rule
parseRule s
  | '"' `elem` s = Eq $ head $ filter (/= '"') s
  | [p1, p2] <- splitOn " | " s = Or (Seq $ map read $ words p1)
                                     (Seq $ map read $ words p2)
  | otherwise = Seq $ map read $ words s

parseRules :: [String] -> M.Map Int Rule
parseRules = M.fromList . map f
  where f s = (read (head spt), parseRule (last spt)) where spt = splitOn ": " s

mkRule0 :: M.Map Int Rule -> Bool -> Parser ()
mkRule0 rules newRules = combineRules (rules M.! 0) *> eof
  where

    combineRules :: Rule -> Parser ()
    -- special case for updated rules
    combineRules (Seq [8, 11]) | newRules
        = do num42 <- length <$> manyTill r42 (try $ lookAhead r31)
             guard (num42 >= 2)
             num31 <- length <$> manyTill r31 (try $ lookAhead eof)
             guard (num31 >= 1 && num31 <= num42-1)

    -- General case
    combineRules (Eq c)     = void $ char c
    combineRules (Or r1 r2) = void $ try (combineRules r1) <|> combineRules r2
    combineRules (Seq is)   = mapM_ combineRules [ rules M.! i | i <- is ]

    r42 = combineRules $ rules M.! 42
    r31 = combineRules $ rules M.! 31

solve :: Bool -> M.Map Int Rule -> [String] -> Int
solve new rules = length . filter isRight . map match
  where match = parse (mkRule0 rules new) ""


main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . uncurry (solve False) =<< input
  putStrLn . ("Star 2: " <>) . show . uncurry (solve True ) =<< input
    where
      input :: IO (M.Map Int Rule, [String])
      input = do
        inp <- lines <$> readFile "input"
        pure (parseRules $ takeWhile (/= "") inp, tail $ dropWhile (/= "") inp)
