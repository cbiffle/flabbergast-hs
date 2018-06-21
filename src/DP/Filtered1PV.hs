{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^4.
--
-- Filter the dictionary to the board.
module DP.Filtered1PV (T) where

import Control.Arrow ((&&&))
import Control.DeepSeq(NFData(..))
import Data.List (foldl', transpose, sort, isSubsequenceOf)
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.Vector as V
import Control.Monad (join)
import Base

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

cheap :: GridOf Char -> String -> GridOf (Maybe Path)
cheap b (c : cs) = foldl' (cheap' b) seed cs
  where
    seed = b `gfor` \i bc -> if bc == c
                               then Just [goPositions b V.! i]
                               else Nothing

search1 :: GridOf Char -> String -> Maybe (String, Path)
search1 b w =
  fmap (\p -> (w, reverse p)) $
  listToMaybe $ catMaybes $ V.toList $ goVec $ cheap b w

data T

instance Solver T where
  type CookedDict T = [(String, String)]
  cookDict = fmap (id &&& sort)

  type CookedBoard T = (GridOf Char, [Char])
  cookBoard = mkGridOf &&& (sort . concat)
                
  solve d (b, cs) =
    let d' = [w | (w, s) <- d, s `isSubsequenceOf` cs]
    in [r | word <- d'
          , r <- maybeToList $ search1 b word
          ]
