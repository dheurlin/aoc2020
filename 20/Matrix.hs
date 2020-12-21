module Matrix
  ( Matrix
  , dims
  , mf
  , (!)
  , transpose
  , flipX
  , flipY
  , rot1
  , fromList
  , getCol
  , getRow
  ) where

import qualified Data.Array as A

-- Small matrix library since the standard one didn't work ---------------------

data Matrix a = M { dims :: (Int, Int), mf :: (Int, Int) -> a }

infixl 9 !
(!) :: Matrix a -> (Int, Int) -> a
(!) (M (mx,my) _) (x,y) | x < 0 || y < 0 || x > mx - 1 || y > my - 1
  = error "Matrix: index out of bounds"
(!) (M _ m) c = m c

instance Show a => Show (Matrix a) where
  show m = unlines [show [ m ! (x, y) | x <- [0..9] ] | y <- [0..9]]

transpose :: Matrix a -> Matrix a
transpose (M (mx,my) m) = M (my, mx) $ \(x,y) -> m (y,x)

flipX :: Matrix a -> Matrix a
flipX (M d@(mx,_) m) = M d $ \(x,y) -> m (mx - 1 - x, y)

flipY :: Matrix a -> Matrix a
flipY (M d@(_,my) m) = M d $ \(x,y) -> m (x, my - 1 - y)

rot1 :: Matrix a -> Matrix a
rot1 = flipX . transpose

fromList :: [[a]] -> Matrix a
fromList as = M (length $ head as, length as) $ \(x, y) -> arr A.! y A.! x
  where arr = A.listArray (0, length as)
            $ map (A.listArray (0, length $ head as)) as

getCol :: Int -> Matrix a -> [a]
getCol i m@(M (_,my) _) = [ m ! (i, y) | y <- [0..my-1]]

getRow :: Int -> Matrix a -> [a]
getRow i m@(M (mx,_) _) = [ m ! (x, i) | x <- [0..mx-1]]

