{-# LANGUAGE LambdaCase #-}

import Numeric    (readInt)
import Data.Char  (isLetter, isDigit)
import Data.Bits
import qualified Data.Map as M

-- Util ------------------------------------------------------------------------

-- Why is this not built in lol
readBin :: String -> Int
readBin = fst . head . readInt 2
                    (\case '1' -> True; '0' -> True; _ -> False)
                    (\case '1' -> 1   ; '0' -> 0)

-- lol
showBin :: Int -> String
showBin n = padBin (showBin' n)
  where
    showBin' n = rest <> show (n .&. 1)
      where
        shifted = shiftR n 1
        rest | shifted == 0 = "0"
             | shifted == 1 = "1"
             | otherwise    = showBin' shifted

padBin :: String -> String
padBin s = replicate (36 - length s) '0' <> s

-- Datatypes -------------------------------------------------------------------

data Instr = Write Int Int
           | SetMask String

type Memory = M.Map Int Int

-- Parsing ---------------------------------------------------------------------

parseInstr :: String -> Instr
parseInstr s = case takeWhile isLetter s of
  "mem"  -> parseMem s
  "mask" -> parseMask s
  _      -> error $ "Parse error: " <> s
  where
    parseMem s = Write addr val
      where
        addr = read . takeWhile isDigit . drop 4 $ s
        val  = read . drop 2 . dropWhile (/= '=') $ s
    parseMask s = SetMask $ drop 2 . dropWhile (/= '=') $ s

-- Star 1 ----------------------------------------------------------------------

mask :: String -> Int -> Int
mask msk n = readBin $ zipWith change msk (showBin n)
  where
    change '0' _ = '0'
    change '1' _ = '1'
    change 'X' b = b

run :: [Instr] -> Memory
run = snd . foldl runInstr ("", M.empty)
  where
    runInstr :: (String, Memory) -> Instr -> (String, Memory)
    runInstr (msk, mem) (Write addr val)
      = (msk, M.insert addr (mask msk val) mem)
    runInstr (_, mem) (SetMask s)
      = (s, mem)

star1 :: [Instr] -> Int
star1 = M.foldr (+) 0 . run

-- Star 2 ----------------------------------------------------------------------

maskAddr :: String -> Int -> [Int]
maskAddr msk n = map readBin $ maskAddr' msk (showBin n)
  where
    maskAddr' :: String -> String -> [String]
    maskAddr' [] []         = pure ""
    maskAddr' ('1':m) (_:b) = map ('1' :) $ maskAddr' m b
    maskAddr' ('0':m) (n:b) = map ( n  :) $ maskAddr' m b
    maskAddr' ('X':m) (_:b) = (\s -> ['0':s, '1':s]) =<< maskAddr' m b

run2 :: [Instr] -> Memory
run2 = snd . foldl runInstr ("", M.empty)
  where
    runInstr :: (String, Memory) -> Instr -> (String, Memory)
    runInstr (_  , mem) (SetMask s) = (s, mem)
    runInstr (msk, mem) (Write addr val) =
      (msk, foldl (\m a -> M.insert a val m) mem $ maskAddr msk addr)

star2 :: [Instr] -> Int
star2 = M.foldr (+) 0 . run2

-- Solution --------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . star1 =<< input
  putStrLn . ("Star 2: " <>) . show . star2 =<< input
  where input = map parseInstr . lines <$> readFile "input"
