import Data.List.Split
import qualified Data.Set as S

type Card = Int

type Deck = [Card]

parseDeck :: [String] -> Deck
parseDeck = map read . tail

parseInput :: [String] -> (Deck, Deck)
parseInput ss = (parseDeck p1, parseDeck p2)
  where [p1,p2] = splitOn [""] ss

play :: Deck -> Deck -> Deck
play [] d = d
play d [] = d
play (c:cs) (d:ds)
  | c > d = play (cs <> [c,d]) ds
  | d > c = play cs (ds <> [d,c])
  | otherwise = play (cs <> [c]) (ds <> [d])

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse

star1 :: Deck -> Deck -> Int
star1 d1 d2 = score $ play d1 d2

type GameState = S.Set (Deck, Deck)

playRec :: GameState -> Deck -> Deck -> (Int, Deck)
playRec _ d [] = (1,d)
playRec _ [] d = (2,d)
playRec s cs ds | (cs,ds) `S.member` s = (1,cs)
playRec s d1@(c:cs) d2@(d:ds) | length cs >= c && length ds >= d = --recursive
  case playRec S.empty (take c cs) (take d ds) of
    (1,_) -> playRec (S.insert (d1,d2) s) (cs <> [c,d]) ds
    (2,_) -> playRec (S.insert (d1,d2) s) cs (ds <> [d,c])
playRec s d1@(c:cs) d2@(d:ds)
  | c > d = playRec (S.insert (d1,d2) s) (cs <> [c,d]) ds
  | c < d = playRec (S.insert (d1,d2) s) cs (ds <> [d,c])

star2 :: Deck -> Deck -> Int
star2 d1 d2 = score . snd $ playRec S.empty d1 d2

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . uncurry star1 =<< input
  putStrLn . ("Star 2: " <>) . show . uncurry star2 =<< input
    where
      input = parseInput . lines <$> readFile "input"
