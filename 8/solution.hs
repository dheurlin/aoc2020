{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.Array

-- Datatypes -------------------------------------------------------------------

data Instr = Nop | Acc Int | Jmp Int
  deriving Show

type Addr = Int
type PC = Int

type Program = Array Int Instr

type State = Int

data ExitStatus = Terminated State | Break State

-- Parsing ---------------------------------------------------------------------

parseInstr :: String -> Instr
parseInstr s = case head (words s) of
  "nop" -> Nop
  "acc" -> Acc readArg
  "jmp" -> Jmp readArg
  where readArg = read . filter (/= '+'). last . words $ s

parseProgram :: String -> Program
parseProgram s = listArray (0, length instrs - 1) instrs
  where instrs = [ parseInstr l | l <- lines s ]

-- Running programs ------------------------------------------------------------

runInstr :: State -> Instr -> (State, Int)
runInstr acc  Nop     = (acc    , 1)
runInstr acc  (Acc i) = (acc + i, 1)
runInstr acc  (Jmp i) = (acc    , i)

run' :: State -> PC -> [Addr] -> Program -> ExitStatus
run' s pc visited prog
  | endOfProg   = Terminated newState
  | shouldBreak = Break newState
  | otherwise   = run' newState newPc (pc : visited) prog

    where
      (newState, offs) = runInstr s (prog ! pc)
      newPc = (pc + offs) `mod` length prog
      shouldBreak = newPc `elem` visited
      endOfProg = pc + offs > length prog - 1

run :: Program -> ExitStatus
run = run' 0 0 []

-- Patching an infinite loop ---------------------------------------------------

replaceNth :: Int -> (a -> Bool) -> a -> Array Int a -> Maybe (Array Int a, Int)
replaceNth n pred repl arr =
  let xs          = elems arr
      ixs         = findIndices pred xs
      ix          = ixs !! (n - 1)
      (pre, post) = splitAt ix xs
      newPost     = repl : tail post
  in if n - 1 > length ixs - 1
        then Nothing
        else Just (listArray (bounds arr) (pre ++ newPost), ix)

patchAndRun :: Program -> Maybe ((Addr, Instr), Int)
patchAndRun program = patchAndRun' 1 program 0
  where
    patchAndRun' n p replaceAddr
      | Terminated state <- run p
        = Just ((replaceAddr, program ! replaceAddr), state)
      | otherwise
        = uncurry (patchAndRun' (n + 1)) =<< replaceNth n isJmp Nop program

    isJmp (Jmp _) = True
    isJmp _       = False

-- Solution --------------------------------------------------------------------

runStar1 :: IO Program -> IO String
runStar1 p = (run <$> p) >>= \case
  Break acc    -> pure $ "Break: acc=" ++ show acc
  Terminated _ -> pure "Didn't break, WTF?"

runStar2 :: IO Program -> IO String
runStar2 p = (patchAndRun <$> p) >>= \case
  Nothing -> pure "Tried substituting all jumps, but we still get a loop :("
  Just ((addr, ins), result) -> pure $ "Terminated with acc=" ++ show result
                                     ++ " by replacing "      ++ show ins
                                     ++ " at #"++ show addr ++" with Nop"

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) =<< runStar1 input
  putStrLn . ("Star 2: " <>) =<< runStar2 input
    where input = parseProgram <$> readFile "input"


