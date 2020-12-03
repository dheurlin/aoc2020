-- ##### Util ##################################################################
--
-- TODO Can be optimized if neeed (lenght and filter do two iterations together)
countOccs :: Eq a => a -> [a] -> Int
countOccs elem = length . filter (== elem)


xor :: Bool -> Bool -> Bool
True  `xor` False = True
False `xor` True  = True
_ `xor` _         = False

-- ##### Solution ##############################################################

data Policy = Policy
  { num1     :: Int
  , num2     :: Int
  , letter   :: Char
  }
  deriving (Eq, Show)


parsePolicy :: String -> (Policy, String)
parsePolicy s = (Policy (read p1) (read p2) l, str) where
  wds = words s
  p1 = takeWhile (/= '-')   $ head wds
  p2 = drop (length p1 + 1) $ head wds
  l = head $ wds !! 1
  str = last wds


checkOldPolicy :: Policy -> String -> Bool
checkOldPolicy (Policy mn mx l) pass = occs >= mn && occs <= mx
  where occs = countOccs l pass


checkNewPolicy :: Policy -> String -> Bool
checkNewPolicy (Policy pos1 pos2 l) pass = fstCorrect `xor` sndCorrect
  where fstCorrect = pass !! (pos1 - 1) == l
        sndCorrect = pass !! (pos2 - 1) == l


main :: IO ()
main = do putStrLn . ("Silver: "<>) . show =<< numCorrect checkOldPolicy
          putStrLn . ("Gold  : "<>) . show =<< numCorrect checkNewPolicy
  where numCorrect checkPolicy = length
                                 . filter id
                                 . map (uncurry checkPolicy . parsePolicy)
                                 . lines <$> readFile "input"
