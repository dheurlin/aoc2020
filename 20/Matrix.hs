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
  , (<~>)
  , (<^>)
  , wide
  , tall
  , grid
  , matrix
  ) where

import qualified Data.Array as A

-- Small matrix library since the standard one didn't work ---------------------

data Matrix a = M { dims :: (Int, Int), mf :: (Int, Int) -> a }

instance Functor Matrix where
  fmap f m@(M (mx,my) _) = M (mx,my) $ \(x,y) -> f (m ! (x,y))

matrix :: (Int, Int) -> ((Int, Int) -> a) -> Matrix a
matrix = M

infixl 7 !
(!) :: Matrix a -> (Int, Int) -> a
(!) (M (mx,my) _) (x,y) | x < 0 || y < 0 || x > mx - 1 || y > my - 1
  = error $ "Matrix: index " <> show (x,y) <> " out of bounds " <> show (mx,my)
(!) (M _ m) i = m i

instance Show a => Show (Matrix a) where
  show m@(M (mx,my) _) =
    unlines [show [ m ! (x, y) | x <- [0..mx-1] ] | y <- [0..my-1]]

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

infixl 8 <~>
(<~>) :: Matrix a -> Matrix a -> Matrix a
(<~>) (M (_,mya) _) (M (_,myb) _)
  | mya /= myb = error "<~>: matrices have different heights"
(<~>) a@(M (mxa,mya) _) b@(M (mxb,_) _) = M (mxa+mxb, mya) $ \(x,y) ->
  if x < mxa then a ! (x,y) else b ! (x-mxa,y)

infixl 7 <^>
(<^>) :: Matrix a -> Matrix a -> Matrix a
(<^>) (M (mxa,_) _) (M (mxb,_) _)
  | mxa /= mxb = error "<~>: matrices have different widths"
(<^>) a@(M (mxa,mya) _) b@(M (_,myb) _) = M (mxa, mya+myb) $ \(x,y) ->
  if y < mya then a ! (x,y) else b ! (x,y-mya)

wide :: [Matrix a] -> Matrix a
wide [] = error "Cannot make W I D E with empty list!"
wide ms = foldl1 (<~>) ms

tall :: [Matrix a] -> Matrix a
tall [] = error "Cannot make T A L L with empty list!"
tall ms = foldl1 (<^>) ms

grid :: [[Matrix a]] -> Matrix a
grid = tall . map wide