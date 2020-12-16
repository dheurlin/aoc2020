import Control.Monad.Trans.State.Lazy
import Debug.Trace

data Instr = N Double
           | S Double
           | E Double
           | W Double
           | L Double
           | R Double
           | F Double
  deriving Show

parseInstr :: String -> Instr
parseInstr s = case head s of
  'N' -> N arg
  'S' -> S arg
  'E' -> E arg
  'W' -> W arg
  'L' -> L arg
  'R' -> R arg
  'F' -> F arg
  _   -> error "Invalid instruction"
  where arg = read . tail $ s

data St = St
  { posX :: Double
  , posY :: Double
  , angle :: Double
  }
  deriving Show

fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180

toDegrees :: Floating a => a -> a
toDegrees grad = grad * 180 / pi

-- North is positive, East is positive

-- -- It's a bitch to work with `mod` and doubles so we do it like this
-- normAngle :: Double -> Double
-- normAngle ang
--   | ang < 0    = normAngle (ang + 360)
--   | ang >= 360 = normAngle (ang - 360)
--   | otherwise  = ang

step :: Instr -> State St ()
step (N dist) = modify (\s -> s {posY = posY s + dist})
step (S dist) = modify (\s -> s {posY = posY s - dist})
step (E dist) = modify (\s -> s {posX = posX s + dist})
step (W dist) = modify (\s -> s {posX = posX s - dist})
step (L ang ) = modify (\s -> s {angle = angle s + ang})
step (R ang ) = modify (\s -> s {angle = angle s - ang})
step (F dist) = modify fwd
  where
    fwd (St x y ang) =
      let xOffs = dist * cos (fromDegrees ang)
          yOffs = dist * sin (fromDegrees ang)
      in St (x + xOffs) (y + yOffs) ang

go :: [Instr] -> (Double, Double)
go instrs = (posX s, posY s)
  where s = execState (mapM step instrs) $ St 0 0 0

star1 :: [Instr] -> Int
star1 is = round $ abs x + abs y
  where (x, y) = go is


data St2 = St2
  { wpX   :: Double
  , wpY   :: Double
  , shipX :: Double
  , shipY :: Double
  }
  deriving Show

step2 :: Instr -> State St2 ()
step2 (N dist) = modify (\s -> s {wpY = wpY s + dist})
step2 (S dist) = modify (\s -> s {wpY = wpY s - dist})
step2 (E dist) = modify (\s -> s {wpX = wpX s + dist})
step2 (W dist) = modify (\s -> s {wpX = wpX s - dist})
step2 (L ang)  = modify $ rotateAround ang
step2 (R ang)  = modify $ rotateAround (-ang)
step2 (F dist) = modify (\s -> s { shipX = shipX s + dist * wpX s
                                   , shipY = shipY s + dist * wpY s })

rotateAround :: Double -> St2 -> St2
rotateAround ang (St2 wpX wpY shipX shipY) =
  let dist    = sqrt $ wpX^2 + wpY^2
      currAng = acos (wpX / dist) * signum wpY
      newAng  = currAng + fromDegrees ang
      newWpX  = dist * cos newAng
      newWpY  = dist * sin newAng
  in St2 newWpX newWpY shipX shipY

go2 :: [Instr] -> (Double, Double)
go2 instrs = (shipX s, shipY s)
  where s = execState (mapM step2 instrs) $ St2 10 1 0 0

star2 :: [Instr] -> Int
star2 is = round $ abs x + abs y
  where (x, y) = go2 is

test :: [Instr]
test =
  [ F 10
  , N 3
  , F 7
  , R 90
  , F 11
  ]

test1 :: [Instr]
test1 = -- Start: 1 1 0 0
  [ F 1 -- 1 1 1 1
  , F 1 -- 1 1 2 2
  , S 1 -- 1 0 2 2
  , F 5 -- 1 0 7 2
  ]

runTest = execState (mapM step2 test1) $ St2 1 1 0 0

-- 20158 too low
main :: IO ()
main = do
  putStrLn . ("Star 1: " <>) . show . star1 =<< input
  putStrLn . ("Star 2: " <>) . show . star2 =<< input
  where
    input = map parseInstr . lines <$> readFile "input"

