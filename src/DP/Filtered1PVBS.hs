{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^5.
--
-- Filter the dictionary to the board, with bytestrings.
module DP.Filtered1PVBS (T) where

import Control.Arrow ((&&&))
import Control.DeepSeq(NFData(..))
import Data.List (foldl', transpose, sort)
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Control.Monad (join)
import Base
import ByteStringUtil

data GridOf a = GO
  { goVec :: V.Vector a
      -- ^ Vector of elements, row-major.
  , goPositions :: !(V.Vector Pos)
      -- ^ Mapping from element indices to positions, for generating paths.
  , goNeighbors :: !(V.Vector [Int])
      -- ^ Mapping from a vector index to the indices of its Cartesian
      -- neighbors.
  }

instance (NFData a) => NFData (GridOf a) where
  rnf (GO v p n) = rnf (v,p,n)

gfor (GO v p n) f = GO (V.imap f v) p n

mkGridOf :: [[a]] -> GridOf a
mkGridOf b =
  let w = boardWidth b
      pos2i (x, y) = x + y * w
      vec = V.fromList $ concat b
      pos = V.fromList $ positions b
      neigh = fmap (map pos2i . nextSteps b) pos
  in GO vec pos neigh

cheap' :: GridOf Char -> GridOf (Maybe Path) -> Char -> GridOf (Maybe Path)
cheap' g@(GO _ gp gn) (GO paths _ _) c =
  g `gfor` \i bc -> listToMaybe $
      [p : path | bc == c
                , ni <- gn V.! i
                , path <- maybeToList $ paths V.! ni
                , let p = gp V.! i
                , p `notElem` path
                ]

cheap :: GridOf Char -> BS.ByteString -> GridOf (Maybe Path)
cheap b cs = BS.foldl' (cheap' b) seed $ BS.tail cs
  where
    seed = b `gfor` \i bc -> if bc == BS.head cs
                               then Just [goPositions b V.! i]
                               else Nothing

search1 :: GridOf Char -> BS.ByteString -> Maybe (String, Path)
search1 b w =
  fmap (\p -> (BS.unpack w, reverse p)) $
  listToMaybe $ catMaybes $ V.toList $ goVec $ cheap b w

data T

instance Solver T where
  type CookedDict T = [(BS.ByteString, BS.ByteString)]
  cookDict = fmap (BS.pack &&& (BS.pack . sort))

  type CookedBoard T = (GridOf Char, BS.ByteString)
  cookBoard = mkGridOf &&& (BS.pack . sort . concat)
                
  solve d (b, cs) =
    let d' = [w | (w, s) <- d, s `isSubsequenceOf` cs]
    in [r | word <- d'
          , r <- maybeToList $ search1 b word
          ]
