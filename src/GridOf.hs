{-# LANGUAGE TypeFamilies #-}

-- | Optimized board representation.
module GridOf  where

import Control.Arrow ((&&&))
import Control.DeepSeq(NFData(..))
import Data.List (foldl', transpose)
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
