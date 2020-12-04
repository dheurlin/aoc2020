{-# LANGUAGE TypeFamilies #-}

import Data.List.Split ( splitOn   )
import Text.Read       ( readMaybe )
import Data.Maybe
import Data.Char       ( isDigit   )
import Control.Monad   ( (>=>)     )

-- Util -----------------------------------------------------------------------

ensure :: (a -> Bool) -> a -> Maybe a
ensure pred x | pred x    = Just x
              | otherwise = Nothing

paragraphs :: String -> [String]
paragraphs = map unwords . splitOn [""] . lines

getFields :: String -> [(String, String)]
getFields = map splitTup . words
  where splitTup str | [a, b] <- splitOn ":" str = (a,b)

-- Types ----------------------------------------------------------------------

data OK = OK
  deriving Show

newtype Passport = Passport OK

-- Generic parsing function ---------------------------------------------------

parsePassport :: [String -> Maybe OK] -> String -> Maybe Passport
parsePassport funs str =
  Passport <$> (byr *> iyr *> eyr *> hgt *> hcl *> ecl *> pid)
    where fields = getFields str
          byr = lookup "byr" fields >>= funs !! 0
          iyr = lookup "iyr" fields >>= funs !! 1
          eyr = lookup "eyr" fields >>= funs !! 2
          hgt = lookup "hgt" fields >>= funs !! 3
          hcl = lookup "hcl" fields >>= funs !! 4
          ecl = lookup "ecl" fields >>= funs !! 5
          pid = lookup "pid" fields >>= funs !! 6

----------------- Star 1 passport ---------------------------------------------

parsePassport1 :: String -> Maybe Passport
parsePassport1 = parsePassport [pure . const OK | _ <- [0..6]]

----------------- Star 2 passport ---------------------------------------------

parsePassport2 :: String -> Maybe Passport
parsePassport2 = parsePassport
  [ validateByr >=> const (pure OK)
  , validateIyr >=> const (pure OK)
  , validateEyr >=> const (pure OK)
  , validateHgt >=> const (pure OK)
  , validateHcl >=> const (pure OK)
  , validateEcl >=> const (pure OK)
  , validatePid >=> const (pure OK)
  ]

  where
    validateYear (min, max) yr =
      ensure ((== 4) . length) yr
        >>= readMaybe
        >>= ensure (>= min)
        >>= ensure (<= max)

    validateByr = validateYear (1920, 2002)
    validateIyr = validateYear (2010, 2020)
    validateEyr = validateYear (2020, 2030)

    validateHgt h
      | unit == "cm" = num >>= ensure (>= 150) >>= ensure (<= 193)
      | unit == "in" = num >>= ensure (>= 59)  >>= ensure (<= 76)
      | otherwise    = Nothing
        where
          unit = dropWhile isDigit h
          num  = readMaybe $ takeWhile isDigit h

    validateHcl ('#':ns) =
      ensure ((== 6) . length) >=> ensure (all (`elem` chars)) $ ns
        where chars = "0123456789abcdef"
    validateHcl _        = Nothing

    validateEcl =
      ensure (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

    validatePid = ensure ((== 9) . length) >=> (readMaybe :: String -> Maybe Int)

------ Solution ---------------------------------------------------------------

main :: IO ()
main = do putStrLn . ("Star 1: " <>) . show =<< ans1
          putStrLn . ("Star 2: " <>) . show =<< ans2
  where
    ans1 = numValid parsePassport1 <$> readFile "input"
    ans2 = numValid parsePassport2 <$> readFile "input"

    numValid :: (String -> Maybe a) -> String -> Int
    numValid parse = length .  mapMaybe parse . paragraphs
