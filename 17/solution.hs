import Data.Maybe
import qualified Data.Map as M

-- Star 1 ---------------------------------------------------------------------

type Coord = (Int, Int, Int)

type Space = (Coord, Coord, M.Map Coord Char)

mkSpace :: [[Char]] -> Space
mkSpace chars = (mn, mx, M.fromList [ ((x,y,0), chars !! y !! x )
                                    | y <- [0..maxY], x <- [0..maxX] ] )
  where maxX = length (head chars) - 1
        maxY = length chars - 1
        mn   = (0, 0, 0)
        mx   = (maxX, maxY, 0)


countAlive :: [Char] -> Int
countAlive = length . filter (== '#')

infixl 9 !

(!) :: Space -> Coord -> Char
(!) (_,_, m) c = M.findWithDefault '.' c m

xPlanes :: Space ->  [Char]
xPlanes ((minX, minY, minZ), (maxX, maxY, maxZ), spc)
  =  [ spc M.! (minX, y, z) | y <- [minY..maxY], z <- [minZ..maxZ] ]
  <> [ spc M.! (maxX, y, z) | y <- [minY..maxY], z <- [minZ..maxZ] ]

yPlanes :: Space ->  [Char]
yPlanes ((minX, minY, minZ), (maxX, maxY, maxZ), spc)
  =  [ spc M.! (x, minY, z) | x <- [minX..maxX], z <- [minZ..maxZ] ]
  <> [ spc M.! (x, maxY, z) | x <- [minX..maxX], z <- [minZ..maxZ] ]

zPlanes :: Space ->  [Char]
zPlanes ((minX, minY, minZ), (maxX, maxY, maxZ), spc)
  =  [ spc M.! (x, y, minZ) | x <- [minX..maxX], y <- [minY..maxY] ]
  <> [ spc M.! (x, y, maxZ) | x <- [minX..maxX], y <- [minY..maxY] ]

extendSpace :: Space -> Space
extendSpace spc@((minX, minY, minZ), (maxX, maxY, maxZ), s)
  = let extend (mn, mx) = (mn - d, mx + d) where d = ((mx - mn) `div` 2) + 1

        (mnX, mxX) = if countAlive (xPlanes spc) > 0
                     then extend (minX, maxX) else (minX, maxX)

        (mnY, mxY) = if countAlive (yPlanes spc) > 0
                     then extend (minY, maxY) else (minY, maxY)

        (mnZ, mxZ) = if countAlive (zPlanes spc) > 0
                     then extend (minZ, maxZ) else (minZ, maxZ)

    in ((mnX, mnY, mnZ), (mxX, mxY, mxZ), s)

neighbors :: Space -> Coord -> [Char]
neighbors spc (x,y,z) = [ spc ! (x + dx, y + dy, z + dz)
                        | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1]
                        , (dx, dy, dz) /= (0,0,0) ]


step :: Space -> Space
step spc = (mn, mx, M.fromList [ ((x,y,z), change (x,y,z))
                               | x <- [mnX..mxX]
                               , y <- [mnY..mxY]
                               , z <- [mnZ..mxZ]])
  where
    extd@(mn@(mnX,mnY,mnZ),mx@(mxX,mxY,mxZ),_) = extendSpace spc

    change :: Coord -> Char
    change coord = case extd ! coord of
      '#' | alive == 2 || alive == 3 -> '#'
          | otherwise                -> '.'
      '.' | alive == 3               -> '#'
          | otherwise                -> '.'

      where alive = countAlive (neighbors extd coord)


star1 :: Space -> Int
star1 spc = countAlive . M.elems $ spc'
  where (_,_,spc') = iterate step spc !! 6

-- Star 2 ---------------------------------------------------------------------
--
type HCoord = (Int, Int, Int, Int)

type HSpace = (HCoord, HCoord, M.Map HCoord Char)

mkHSpace :: [[Char]] -> HSpace
mkHSpace chars = (mn, mx, M.fromList [ ((x,y,0,0), chars !! y !! x )
                                    | y <- [0..maxY], x <- [0..maxX] ] )
  where maxX = length (head chars) - 1
        maxY = length chars - 1
        mn   = (0, 0, 0, 0)
        mx   = (maxX, maxY, 0, 0)


infixl 9 ?

(?) :: HSpace -> HCoord -> Char
(?) (_,_, m) c = M.findWithDefault '.' c m

xHPlanes :: HSpace ->  [Char]
xHPlanes ((minX, minY, minZ, minW), (maxX, maxY, maxZ, maxW), spc)
  =  [ spc M.! (minX,y,z,w) | y <- [minY..maxY], z <- [minZ..maxZ], w <- [minW..maxW] ]
  <> [ spc M.! (maxX,y,z,w) | y <- [minY..maxY], z <- [minZ..maxZ], w <- [minW..maxW] ]

yHPlanes :: HSpace ->  [Char]
yHPlanes ((minX, minY, minZ, minW), (maxX, maxY, maxZ, maxW), spc)
  =  [ spc M.! (x,minY,z,w) | x <- [minX..maxX], z <- [minZ..maxZ], w <- [minW..maxW] ]
  <> [ spc M.! (x,maxY,z,w) | x <- [minX..maxX], z <- [minZ..maxZ], w <- [minW..maxW] ]

zHPlanes :: HSpace ->  [Char]
zHPlanes ((minX, minY, minZ, minW), (maxX, maxY, maxZ, maxW), spc)
  =  [ spc M.! (x,y,minZ,w) | x <- [minX..maxX], y <- [minY..maxY], w <- [minW..maxW] ]
  <> [ spc M.! (x,y,maxZ,w) | x <- [minX..maxX], y <- [minY..maxY], w <- [minW..maxW] ]

wHPlanes :: HSpace ->  [Char]
wHPlanes ((minX, minY, minZ, minW), (maxX, maxY, maxZ, maxW), spc)
  =  [ spc M.! (x,y,z,minW) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ] ]
  <> [ spc M.! (x,y,z,maxW) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ] ]

extendHSpace :: HSpace -> HSpace
extendHSpace spc@((minX, minY, minZ, minW), (maxX, maxY, maxZ, maxW), s)
  = let extend (mn, mx) = (mn - d, mx + d) where d = ((mx - mn) `div` 2) + 1

        (mnX, mxX) = if countAlive (xHPlanes spc) > 0
                     then extend (minX, maxX) else (minX, maxX)

        (mnY, mxY) = if countAlive (yHPlanes spc) > 0
                     then extend (minY, maxY) else (minY, maxY)

        (mnZ, mxZ) = if countAlive (zHPlanes spc) > 0
                     then extend (minZ, maxZ) else (minZ, maxZ)

        (mnW, mxW) = if countAlive (wHPlanes spc) > 0
                     then extend (minW, maxW) else (minW, maxW)

    in ((mnX, mnY, mnZ, mnW), (mxX, mxY, mxZ, mxW), s)

hneighbors :: HSpace -> HCoord -> [Char]
hneighbors spc (x,y,z,w) = [ spc ? (x + dx, y + dy, z + dz, w + dw)
                        | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1]
                        , (dx, dy, dz, dw) /= (0,0,0,0) ]


hstep :: HSpace -> HSpace
hstep spc = (mn, mx, M.fromList [ ((x,y,z,w), change (x,y,z,w))
                               | x <- [mnX..mxX]
                               , y <- [mnY..mxY]
                               , z <- [mnZ..mxZ]
                               , w <- [mnW..mxW]])
  where
    extd@(mn@(mnX,mnY,mnZ,mnW),mx@(mxX,mxY,mxZ,mxW),_) = extendHSpace spc

    change :: HCoord -> Char
    change coord = case extd ? coord of
      '#' | alive == 2 || alive == 3 -> '#'
          | otherwise                -> '.'
      '.' | alive == 3               -> '#'
          | otherwise                -> '.'

      where alive = countAlive (hneighbors extd coord)


star2 :: HSpace -> Int
star2 spc = countAlive . M.elems $ spc'
  where (_,_,spc') = iterate hstep spc !! 6

main :: IO ()
main = do
  -- inp1 <- mkSpace . lines <$> readFile "input"
  -- putStrLn . ("Star 1: " <>) . show $ star1 inp1

  inp2 <- mkHSpace . lines <$> readFile "input"
  putStrLn . ("Star 2: " <>) . show $ star2 inp2
