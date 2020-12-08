{-# LANGUAGE ParallelListComp, LambdaCase #-}

import Text.Printf ( printf )
import Data.List

-- Datatypes -------------------------------------------------------------------

data Instr = Nop | Acc Int | Jmp Int
  deriving Show

type Addr = Int

data ProgramEntry = PE { pAddr :: Addr, pInstr :: Instr, pVisited :: Bool }

instance Show ProgramEntry where
  show (PE addr inst v) = printf "#%d | %-8s, %s" addr (show inst)
                                (if v then "visited" else "not visited")

setVisited :: ProgramEntry -> ProgramEntry
setVisited (PE a i _) = PE a i True

type Program = [ProgramEntry]

type Breakpoint = State -> ProgramEntry -> Bool

type State = Int

data ExitStatus = Terminated Int
                | Break ProgramEntry ProgramEntry Int

instance Show ExitStatus where
  show (Terminated s)      = printf "Terminated with acc=%d" s
  show (Break prev curr s) =
    printf "\nBreak!\nPrevious: %s\nCurrent : %s\nacc=%d" (show prev) (show curr) s

-- Parsing ---------------------------------------------------------------------

parseInstr :: String -> Instr
parseInstr s = case head (words s) of
  "nop" -> Nop
  "acc" -> Acc readArg
  "jmp" -> Jmp readArg
  where readArg = read . filter (/= '+'). last . words $ s

parseProgram :: String -> Program
parseProgram s = [PE n (parseInstr l) False | l <- lines s | n <- [0..]]

-- Running programs ------------------------------------------------------------

jump :: Int -> Program -> Program
jump offset ps =
  iterate (rotate $ signum offset) ps !! abs offset
    where
      rotate 1    (a:as) = as ++ [a]
      rotate (-1) as     = last as : take (length as - 1) as

runInstr :: State -> ProgramEntry -> (State, Int)
runInstr acc  (PE _ Nop _)     = (acc, 1)
runInstr acc  (PE _ (Acc i) _) = (acc + i, 1)
runInstr acc  (PE _ (Jmp i) _) = (acc, i)

run' :: [Breakpoint] -> State -> Program -> ExitStatus
run' bs s (p:ps)
  | pAddr p + offs > length ps = Terminated newState
  | hitBreakpoint              = Break p (head newProgram) newState
  | otherwise                  = run' bs newState newProgram

    where
      (newState, offs) = runInstr s p
      newProgram = jump offs (setVisited p : ps)
      hitBreakpoint = any (($ head newProgram) . ($ newState)) bs

run :: [Breakpoint] -> Program -> ExitStatus
run bs = run' bs 0

-- Patching an infinite loop ---------------------------------------------------

substituteNth :: Int -> (a -> Bool) -> (a -> a) -> [a] -> Maybe ([a], a)
substituteNth n pred f xs =
  let ixs         = findIndices pred xs
      ix          = ixs !! (n - 1)
      (pre, post) = splitAt ix xs
      newPost     = f (head post) : tail post
  in if n - 1 > length ixs - 1
        then Nothing
        else Just (pre ++ newPost, head post)

patchAndRun :: Program -> Maybe (ProgramEntry, Int)
patchAndRun program = patchAndRun' 1 program (head program)
  where
    patchAndRun' n p replaced = case run [const pVisited] p of
      Terminated state -> Just (replaced, state)
      Break {}         -> uncurry (patchAndRun' (n + 1))
                            =<< substituteNth n isJmp toNop program

    isJmp (PE _ (Jmp _) _) = True
    isJmp _                = False

    toNop (PE a _ _) = PE a Nop False

-- Solution --------------------------------------------------------------------

runStar1 :: IO Program -> IO String
runStar1 p = (run [const pVisited] <$> p) >>= \case
  Break _ _ acc  -> pure $ "Break: acc=" ++ show acc
  Terminated _   -> pure "Didn't break, WTF?"

runStar2 :: IO Program -> IO String
runStar2 p = (patchAndRun <$> p) >>= \case
  Nothing -> pure "Tried substituting all jumps, but we still get a loop :("
  Just (PE addr ins _, result) -> pure $ printf
    "Terminated with acc=%d by replacing %s at #%d with Nop" result (show ins) addr

rs2 = runStar2 (parseProgram <$> readFile "test1")

main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) =<< runStar1 input
  putStrLn . ("Star 2: " <>) =<< runStar2 input
    where input = parseProgram <$> readFile "input"

testProgram :: Program
testProgram =
  [ PE 0 Nop False
  , PE 1 (Acc 1) False
  , PE 2 (Jmp 4) False
  , PE 3 (Acc 3) False
  , PE 4 (Jmp (-3)) False
  , PE 5 (Acc (-99)) False
  , PE 6 (Acc 1) False
  , PE 7 (Jmp (-4)) False
  , PE 8 (Acc 6) False
  ]

runTest = run [const pVisited] testProgram
