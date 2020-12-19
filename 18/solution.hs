import Text.Parsec
import Text.Parsec.String

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Num Int
          | InParens Expr

evalExpr :: Expr -> Int
evalExpr (Add e1 e2)  = evalExpr e1 + evalExpr e2
evalExpr (Mul e1 e2)  = evalExpr e1 * evalExpr e2
evalExpr (InParens e) = evalExpr e
evalExpr (Num n)      = n

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

term :: Parser Expr -> Parser Expr
term p = parseParens p <|> parseNum

parseNum :: Parser Expr
parseNum = Num . read <$> lexeme (many digit)

parseParens :: Parser Expr -> Parser Expr
parseParens p = InParens <$> (lexeme (char '(') *> p <* lexeme (char ')'))

parseExpr1 :: Parser Expr
parseExpr1 = term parseExpr1 `chainl1` op
  where op = (Add <$ lexeme (char '+')) <|> (Mul <$ lexeme (char '*'))

parseExpr2 :: Parser Expr
parseExpr2 = term parseExpr2 `chainl1` (Add <$ lexeme (char '+'))
                             `chainl1` (Mul <$ lexeme (char '*'))

solve :: [Expr] -> Int
solve = sum . map evalExpr

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . fmap solve =<< input1
  putStrLn . ("Star 2: " <>) . show . fmap solve =<< input2
    where
      input1 = mapM (parse parseExpr1 "input") . lines <$> readFile "input"
      input2 = mapM (parse parseExpr2 "input") . lines <$> readFile "input"
