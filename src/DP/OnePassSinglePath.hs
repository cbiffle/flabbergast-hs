{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^2.
--
-- This exploits the fact that a word only counts for points once per board,
-- by not recording multiple paths per square.
module DP.OnePassSinglePath (T) where

import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.ByteString.Char8 as BS
import Base

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
            , ni <- neighborIndices b i
            , path <- maybeToList $ paths `at` ni
            , i `notElem` path]

-- | Searches 'b' for an instance of 'w', returning one if found.
search1 :: RawBoard -> BS.ByteString -> Maybe (BS.ByteString, Path)
search1 b w = fmap (\p -> (w, ipath b $ reverse p)) $
              listToMaybe $ catMaybes $ ungrid $
              BS.foldl' (step b) seed (BS.tail w)
  where
    seed = b `gfor` \i bc -> if bc == BS.head w then Just [i] else Nothing

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | (word, _) <- d, r <- maybeToList $ search1 b word]
