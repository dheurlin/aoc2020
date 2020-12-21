{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

import Data.Char
import Data.List.Split
import Data.Maybe
import Control.Monad
import Data.List (subsequences, nub, delete)
import qualified Data.Map as M

-- import Debug.Trace
import Text.Printf

import Matrix

-- Star 1 ----------------------------------------------------------------------

data Tile = T { tNum :: Int, tMat :: Matrix Char }
instance Eq Tile where (T n _) == (T m _) = n == m

parseTiles :: [String] -> [Tile]
parseTiles = map parseTile . splitOn [""]
  where
    parseTile ss = T tileNo $ fromList tile
      where tileNo = read . filter isDigit $ head ss
            tile   = tail ss

type Image = M.Map (Int, Int) Tile

imgBounds :: Image -> ((Int, Int), (Int, Int))
imgBounds img = ((minX, maxX), (minY, maxY))
  where
    (minX, maxX, minY, maxY) = (minimum xs, maximum xs, minimum ys, maximum ys)
    (xs, ys) = unzip $ M.keys img

data Dir = North | South | East | West
  deriving (Eq, Show)

opposite :: Dir -> Dir
opposite North = South
opposite South = North
opposite East = West
opposite West = East

side :: Tile -> Dir -> [Char]
side t South = getRow 9 $ tMat t
side t North = getRow 0 $ tMat t
side t West  = getCol 0 $ tMat t
side t East  = getCol 9 $ tMat t

neighbors :: Image -> (Int, Int) -> [(Dir, Tile)]
neighbors i (x,y) = catMaybes [n, s, e, w]
  where
    n = (North,) <$> M.lookup (x,y-1) i
    s = (South,) <$> M.lookup (x,y+1) i
    e = (East ,) <$> M.lookup (x+1,y) i
    w = (West ,) <$> M.lookup (x-1,y) i

-- All available slots in an image
slots :: Image -> [(Int, Int)]
slots i = nub $ catMaybes $ concatMap (uncurry emptyNeighbors) (M.keys i)
  where
    emptyNeighbors x y = [ coord <$ guard (M.notMember coord i)
                         | coord <- [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ] ]

-- Does a tile fit into this slot in the image?
fits :: Tile -> Image -> (Int, Int) -> Bool
fits t i coord = all isOk (neighbors i coord)
  where
    isOk (d, n) = side n (opposite d) == side t d

-- All transformations of a matrix
allTrans :: Matrix a -> [Matrix a]
allTrans m = [ apAll fs m | fs <- allTransfs ]
    where
      apAll      = foldr (.) id
      allTransfs = concat $ [ map (rot <>) flips | rot <- rots ]
      rots       = [replicate n rot1 | n <- [0..3] ]
      flips      = subsequences [flipX, flipY]

-- All transformed versions of a tile
transformations :: Tile -> [Tile]
transformations (T num m) = map (T num) $ allTrans m

insertTiles :: Image -> [Tile] -> [Image]
insertTiles img [] = [img]
insertTiles img ts = concat
  [ insertTiles (M.insert slot trans img) (delete tile ts)
  | slot <- slots img, tile <- ts, trans <- transformations tile
  , fits trans img slot
  ]

solve :: [Tile] -> Image
solve tiles = head $ insertTiles (M.singleton (0,0) $ head tiles) (tail tiles)

star1 :: Image -> Int
star1 image =
  product [ tNum $ image M.! (x,y) | x <- [minX, maxX], y <- [minY, maxY] ]
  where
    ((minX, maxX), (minY, maxY)) = imgBounds image

-- Star 2 ----------------------------------------------------------------------

rmBorders :: Matrix a -> Matrix a
rmBorders m = subMatrix (1,1) (mx-1,my-1) m
  where (mx,my) = dims m

mkImage :: Image -> Matrix Char
mkImage i =
  grid [[ rmBorders . tMat $ i M.! (x,y) | x <- [minX..maxX]] | y <- [minY..maxY]]
    where ((minX, maxX), (minY, maxY)) = imgBounds i

monster :: Matrix Char
monster = fromList
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

-- Is this region a monster?
isMatch :: Matrix (Char, Char) -> Bool
isMatch m = foldMat (&&) True $ fmap (\case ('#', '.') -> False; _ -> True) m

regions :: Int -> Int -> Matrix Char -> [Matrix Char]
regions w h m =
  [ subMatrix (x,y) (x+w,y+h) m | x <- [0..mx-1-w], y <- [0..my-1-h] ]
    where (mx,my) = dims m

-- Number of monsters for a certain transormation of the monster
matches :: Matrix Char -> Matrix Char -> Int
matches m monster = length (filter id $ map isMatch regs)
  where
    regs = map (matZip monster) $ uncurry regions (dims monster) m

-- Total number of monsters
totalMatches :: Matrix Char -> Int
totalMatches m = maximum . map (matches m) $ allTrans monster

star2 :: Image -> Int
star2 i = matCount (== '#') m - 15 * totalMatches m
  where m = mkImage i

main :: IO ()
main = do
  img <- solve <$> input
  putStrLn . ("Star 1: " <>) . show $ star1 img
  putStrLn . ("Star 2: " <>) . show $ star2 img
  where
    input = parseTiles . lines <$> readFile "input"
