{-# LANGUAGE LambdaCase #-}

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as M

type Color     = String
type ColorMap  = M.Map Color [(Int, Color)]

parseLine :: Parser (Color, [(Int, Color)])
parseLine = (,) <$> parseBag <* string " contain " <*>
                    (parseBagNums <|> [] <$ string "no other bags.")
    where
      parseBagNums = parseBagNum `sepBy1` string ", " <* char '.'
      parseBagNum  = (,) <$> (read <$> many1 digit <* space) <*> parseBag
      parseBag = manyTill anyChar (try $ space *>
                                  (string "bag" *> optional (string "s")))
parseFile :: IO ColorMap
parseFile = do
  mapM (parse parseLine "input") . lines <$> readFile "input" >>= \case
      Right stuff -> pure $ M.fromList stuff
      Left  e     -> fail (show e)

numCanContain :: String -> ColorMap -> Int
numCanContain target rs =
  length . filter id $ map (uncurry accessibleFrom) withoutTarget
    where
      withoutTarget = M.toList $ M.delete target rs

      accessibleFrom bag cols
        | target == bag = True
        | otherwise = any (\(_, col) -> accessibleFrom col (rs M.! col)) cols

countBagsFrom :: String -> ColorMap -> Int
countBagsFrom elem rs =
  numBags + sum [n * countBagsFrom col rs | (n, col) <- cols]
    where cols    = rs M.! elem
          numBags = sum $ map fst cols

main = do
  putStrLn  . ("Star 1: " <>) . show =<< ans1
  putStrLn  . ("Star 2: " <>) . show =<< ans2
  where
    ans1 = numCanContain "shiny gold" <$> parseFile
    ans2 = countBagsFrom "shiny gold" <$> parseFile

