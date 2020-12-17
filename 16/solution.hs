import           Data.List.Split
import           Data.List
import           Control.Monad
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

-- Types -----------------------------------------------------------------------

type Range  = (Int, Int)

type Ticket = [Int]

data Class  = Class { cName :: String
                    , r1    :: Range
                    , r2    :: Range
                    }

-- Parsing ---------------------------------------------------------------------

parseClass :: String -> Class
parseClass s = Class name r1 r2
  where
    name = takeWhile (/= ':') s
    wds = words s
    r1 = parseRange $ wds !! (length wds - 3)
    r2 = parseRange $ last wds
    parseRange r = ( read $ takeWhile (/= '-') r
                   , read $ tail $ dropWhile (/= '-') r)

parseClasses :: [String] -> [Class]
parseClasses = map parseClass . takeWhile (/= "")

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseMyTicket :: [String] -> Ticket
parseMyTicket = parseTicket . (!! 1) . dropWhile (/= "your ticket:")

parseNearby :: [String] -> [Ticket]
parseNearby = map parseTicket . tail . dropWhile (/= "nearby tickets:")

-- Lib -------------------------------------------------------------------------

checkRanges :: Class -> Int -> Bool
checkRanges c i = checkRange (r1 c) i || checkRange (r2 c) i
  where
    checkRange (lower, upper) i = i >= lower && i <= upper

invalidAll :: [Class] -> Int -> Bool
invalidAll cls i = not $ any (`checkRanges` i) cls

isInvalid :: [Class] -> Ticket -> Bool
isInvalid cls = any (invalidAll cls)

-- Star 1 ----------------------------------------------------------------------

invalidAll' :: [Class] -> Int -> Maybe Int
invalidAll' cls i = i <$ guard (invalidAll cls i)

getInvalid :: [Class] -> Ticket -> [Int]
getInvalid cls = mapMaybe (invalidAll' cls)

star1 :: [Class] -> [Ticket] -> Int
star1 cls = sum . concatMap (getInvalid cls)

-- Star 2 ----------------------------------------------------------------------

type Candidates = M.Map String (S.Set Int)

-- Finds the columns that could fit a given class
getCands :: Class -> [Ticket] -> Candidates
getCands cls tickets = M.singleton (cName cls) candidates
  where
    cols             = zip [0..] $ transpose tickets
    possibleColumn c = all (checkRanges c)
    candidates = S.fromList [ ix | (ix, col) <- cols, possibleColumn cls col]

-- Uniquely maps each class name to its correct column in a ticket
solveFields :: [Class] -> [Ticket] -> M.Map String Int
solveFields clss tickets = fst $ go (M.empty, candsAll)
  where
    candsAll = M.unions $ map (`getCands` tickets) clss
    -- Ideitifes the mapping for one candidate at a time
    go :: (M.Map String Int, Candidates) -> (M.Map String Int, Candidates)
    go (soFar,rest) | M.size rest == 0 = (soFar, rest)
    go (soFar,rest) =
      let (name, fewest) = head . sortOn (length . snd) $ M.toList rest
          chosen         = head $ S.elems fewest
          newRest        = M.map (S.delete chosen) $ M.delete name rest
      in go (M.insert name chosen soFar, newRest)

star2 :: [Class] -> [Ticket] -> Ticket -> Int
star2 clss others mine
  = M.foldr (*) 1 . M.map (mine !!)
  . M.filterWithKey (\k _ -> head (words k) == "departure")
  $ solveFields clss others


-- Solution --------------------------------------------------------------------

main :: IO ()
main = do
  (classes, mine, others) <- input
  let validOthers = filter (not . isInvalid classes) others

  putStrLn . ("Star 1: " <>) . show $ star1 classes others
  putStrLn . ("Star 2: " <>) . show $ star2 classes validOthers mine

    where
      input :: IO ([Class], Ticket, [Ticket])
      input = do
        inp <- lines <$> readFile "input"
        pure (parseClasses inp, parseMyTicket inp, parseNearby inp)
