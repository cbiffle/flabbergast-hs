{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Base where

import Control.Monad (replicateM, forM_)
import Test.QuickCheck
import Data.List (group, sort, intercalate, (\\))
import Data.List.Split (chunksOf)

type RawDictionary = [String]
type RawBoard = [[Char]]
type Pos = (Int, Int)
type Path = [Pos]

class Solver s where
  type CookedDict s
  type CookedBoard s
  cookDict :: RawDictionary -> CookedDict s
  cookBoard :: RawBoard -> CookedBoard s
  solve :: CookedDict s -> CookedBoard s -> [(String, Path)]

boardWidth, boardHeight :: [[a]] -> Int
boardWidth = length . head
boardHeight = length

b `tile` (x, y) = b !! y !! x

positions :: [[a]] -> [Pos]
positions b = [(x, y) | y <- [0 .. boardHeight b - 1]
                      , x <- [0 .. boardWidth b - 1]]

nextSteps :: [[a]] -> Pos -> [Pos]
nextSteps b (x, y) =
  let w = boardWidth b
      h = boardHeight b
  in [(x', y') | x' <- [max 0 (x-1) .. min (w-1) (x+1)]
               , y' <- [max 0 (y-1) .. min (h-1) (y+1)]
               , x' /= x || y' /= y
               ]

mapWithPos :: (Pos -> a -> b) -> [[a]] -> [[b]]
mapWithPos f xs = map f' $ zip [0..] $ map (zip [0 :: Int ..]) xs
  where f' (y, xs) = flip map xs $ \(x, a) -> f (x, y) a

arbitraryBoard :: [String] -> Int -> Int -> Gen RawBoard
arbitraryBoard ds w h = do
  ds <- replicateM (w * h) $ elements ds
  cs <- mapM elements ds
  pure $ chunksOf w cs

dice92 = [ "LRYTTE", "VTHRWE", "EGHWNE", "SEOTIS"
         , "ANAEEG", "IDSYTT", "OATTOW", "MTOICU"
         , "AFPKFS", "XLDERI", "HCPOAS", "ENSIEU"
         , "YLDEVR", "ZNRNHL", "NMIQHU", "OBBAOJ"
         ]

newtype Boggle92 = Boggle92 RawBoard

instance Arbitrary Boggle92 where
  arbitrary = Boggle92 <$> arbitraryBoard dice92 4 4

--------------------------------------------------------------------------------
-- Rules checking

-- | Checks that the 'word' claimed to be present at 'path' in 'b' is actually
-- there.
checkWord :: RawBoard -> String -> Path -> Either String ()
checkWord b word path = forM_ (zip word path) $ \(c, p) ->
  if b `tile` p == c
    then pure ()
    else Left $ concat ["Board does not contain '", [c], "' at ", show p]

-- | Checks that the path is internally valid, i.e. does not self-intersect.
checkPath :: Path -> Either String ()
checkPath ps = forM_ (group $ sort ps) $ \pgroup ->
  case pgroup of
    [_] -> pure ()
    (p : _) -> Left $ concat ["Position ", show p, " appears "
                             , show (length pgroup), " times in path"]

checkLegalWords :: RawDictionary -> [String] -> Either String ()
checkLegalWords d ws = case ws \\ d of
  [] -> pure ()
  illegal -> Left $ "Illegal words: " ++ intercalate " " illegal

checkPlay :: RawDictionary -> RawBoard -> [(String, Path)] -> Either String ()
checkPlay d b results = do
  mapM_ (uncurry (checkWord b)) results
  mapM_ checkPath $ map snd results
  checkLegalWords d $ map fst results
