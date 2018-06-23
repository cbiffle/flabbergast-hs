{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
module Base where

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM, forM_)
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Data.List (group, sort, intercalate, (\\))
import Data.List.Split (chunksOf)

type Pos = (Int, Int)
type Path = [Pos]
type IPath = [Int]

type RawDictionary = [BS.ByteString]

type Results = [(BS.ByteString, Path)]

data GridOf a = GO
  { goVec :: V.Vector a
      -- ^ Vector of elements, row-major.
  , goPositions :: !(V.Vector Pos)
      -- ^ Mapping from element indices to positions, for generating paths.
  , goNeighbors :: !(V.Vector [Int])
      -- ^ Mapping from a vector index to the indices of its Cartesian
      -- neighbors.
  , goWidth :: Int
  }

instance (NFData a) => NFData (GridOf a) where
  rnf (GO v p n i) = rnf (v,p,n,i)

instance Show (GridOf Char) where
  show g = unlines $ chunksOf (goWidth g) $ V.toList $ goVec g

gfor (GO v p n w) f = GO (V.imap f v) p n w

mkGridOf :: [BS.ByteString] -> GridOf Char
mkGridOf b =
  let w = BS.length $ head b
      h = length b
      pos2i (x, y) = x + y * w
      vec = V.fromList $ BS.unpack $ BS.concat b
      pos = V.fromList $ [(x, y) | y <- [0 .. h - 1]
                                 , x <- [0 .. w - 1]]
      neigh = fmap (map pos2i . nextSteps w h) pos
  in GO vec pos neigh w

indices :: GridOf a -> [Int]
indices g = [0 .. V.length (goVec g) - 1]

neighborIndices :: GridOf a -> Int -> [Int]
neighborIndices g i = goNeighbors g V.! i

ungrid :: GridOf a -> [a]
ungrid = V.toList . goVec

ipath :: GridOf a -> IPath -> Path
ipath g = map (goPositions g V.!)

type RawBoard = GridOf Char

class Solver s where
  type CookedDict s
  type CookedBoard s
  cookDict :: RawDictionary -> CookedDict s
  cookBoard :: RawBoard -> CookedBoard s
  solve :: CookedDict s -> CookedBoard s -> Results

cookAndSolve :: forall s. (Solver s)
             => RawDictionary -> RawBoard -> Results
cookAndSolve d b = solve @s (cookDict @s d) (cookBoard @s b)

boardWidth, boardHeight :: RawBoard -> Int
boardWidth = goWidth
boardHeight b = V.length (goVec b) `div` goWidth b
  -- TODO: these are probably obsolescent.

b `at` i = goVec b V.! i

b `tile` p = let Just i = V.elemIndex p $ goPositions b
             in goVec b V.! i

nextSteps :: Int -> Int -> Pos -> [Pos]
nextSteps w h (x, y) =
   [(x', y') | x' <- [max 0 (x-1) .. min (w-1) (x+1)]
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
  pure $ mkGridOf $ map BS.pack $ chunksOf w cs

dice92 = [ "LRYTTE", "VTHRWE", "EGHWNE", "SEOTIS"
         , "ANAEEG", "IDSYTT", "OATTOW", "MTOICU"
         , "AFPKFS", "XLDERI", "HCPOAS", "ENSIEU"
         , "YLDEVR", "ZNRNHL", "NMIQHU", "OBBAOJ"
         ]

newtype Boggle92 = Boggle92 RawBoard

instance Arbitrary Boggle92 where
  arbitrary = Boggle92 <$> arbitraryBoard dice92 4 4

newtype SizedBoggle92 = SizedBoggle92 RawBoard
  deriving (Show)

instance Arbitrary SizedBoggle92 where
  arbitrary = do
    s <- getSize
    SizedBoggle92 <$> arbitraryBoard dice92 s s


newtype FakeDict = FakeDict RawDictionary
  deriving (Show)

instance Arbitrary FakeDict where
  arbitrary = do
    n <- getSize
    fmap (FakeDict . map (BS.pack . head) . group . sort) $
      resize 1000 $ listOf $ do
        wl <- (+3) . getNonNegative <$> resize ((n ^ 2) - 3) arbitrary
        take wl <$> shuffle (concat dice92)

--------------------------------------------------------------------------------
-- Rules checking

-- | Checks that the 'word' claimed to be present at 'path' in 'b' is actually
-- there.
checkWord :: RawBoard -> BS.ByteString -> Path -> Either String ()
checkWord b word path = forM_ (zip (BS.unpack word) path) $ \(c, p) ->
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

checkLegalWords :: RawDictionary -> [BS.ByteString] -> Either String ()
checkLegalWords d ws = case ws \\ d of
  [] -> pure ()
  illegal -> Left $ "Illegal words: " ++ intercalate " " (map BS.unpack illegal)

checkUnique :: [BS.ByteString] -> Either String ()
checkUnique words =
  case map head $ filter (\dupes -> length dupes > 1) $ group $ sort words of
    [] -> pure ()
    dupes -> Left $ "Duplicate plays: " ++ intercalate " " (map BS.unpack dupes)

checkPlay :: RawDictionary -> RawBoard -> Results -> Either String ()
checkPlay d b results = do
  mapM_ (uncurry (checkWord b)) results
  mapM_ checkPath $ map snd results
  checkUnique (map fst results)
  checkLegalWords d $ map fst results
