{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming: exactly OnePassSinglePath, but using GridOf.
--
-- This reduces board access time during solving, and the DP algorithms do
-- a *lot* of board access -- this reduces runtime by 45% on a 4x4 board.
module DP.OnePassSinglePathVec (T) where

import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.Vector as V
import Base
import GridOf

type IPath = [Int]

-- | One step of the search, phrased as a fold. Given a grid of valid paths for
-- a word prefix, tries to extend those paths to include a neighboring instance
-- of character 'c', or discard them if they cannot be extended.
--
-- If duplicate paths reach a tile, their futures are identical, so we
-- arbitrarily choose one to survive and discard the rest without loss of
-- generality. (This is implicit in the use of 'listToMaybe'.)
step :: GridOf Char -> GridOf (Maybe IPath) -> Char -> GridOf (Maybe IPath)
step b paths c = b `gfor` \i bc ->
  listToMaybe $
  [i : path | bc == c
            , ni <- goNeighbors b V.! i
            , path <- maybeToList $ goVec paths V.! ni
            , i `notElem` path]

-- | Searches 'b' for an instance of 'w', returning one if found.
search1 :: GridOf Char -> String -> Maybe (String, Path)
search1 b w = fmap (\p -> (w, map (goPositions b V.!) $ reverse p)) $
              listToMaybe $ catMaybes $ V.toList $ goVec $
              foldl' (step b) seed (tail w)
  where
    seed = b `gfor` \i bc -> if bc == head w then Just [i] else Nothing

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = id

  type CookedBoard T = GridOf Char
  cookBoard = mkGridOf

  solve d b = [r | word <- d, r <- maybeToList $ search1 b word]
