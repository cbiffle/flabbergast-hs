{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^4.
--
-- Uses the one-pass single-path algorithm, but filters the dictionary to the
-- board. Since the scaling of the runtime is dominated by the length of the
-- dictionary, this cuts runtime significantly (~75%).
module DP.Filtered1PV (T) where

import Control.Arrow ((&&&))
import Control.DeepSeq(NFData(..))
import Data.List (foldl', transpose, sort)
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import Control.Monad (join)
import Base
import ByteStringUtil

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
search1 :: GridOf Char -> BS.ByteString -> Maybe (BS.ByteString, Path)
search1 b w = fmap (\p -> (w, map (goPositions b V.!) $ reverse p)) $
              listToMaybe $ catMaybes $ V.toList $ goVec $
              BS.foldl' (step b) seed (BS.tail w)
  where
    seed = b `gfor` \i bc -> if bc == BS.head w then Just [i] else Nothing

data T

instance Solver T where
  -- Sort the characters in the board so we can do cheap subsequence tests.
  type CookedBoard T = (GridOf Char, BS.ByteString)
  cookBoard = id &&& (BS.pack . sort . ungrid)
                
  solve d (b, cs) =
    let d' = [w | (w, s) <- d, s `isSubsequenceOf` cs]
    in [r | word <- d', r <- maybeToList $ search1 b word]
